package controllers

import play.api._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.Future
import play.api.libs.json._

import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current

import utils.Time

import java.net.URL
import java.math.BigInteger
import java.security.SecureRandom

import com.google.api.client.auth.oauth2.TokenResponseException
import com.google.api.client.googleapis.auth.oauth2.{GoogleCredential, GoogleAuthorizationCodeTokenRequest, GoogleTokenResponse}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.oauth2.Oauth2
import com.google.api.services.oauth2.model.Tokeninfo
import com.google.gdata.client.appsforyourdomain.UserService
import com.google.gdata.client.authn.oauth.GoogleOAuthParameters
import com.google.gdata.client.authn.oauth.OAuthHmacSha1Signer
import com.google.gdata.client.authn.oauth.OAuthParameters.OAuthType
import com.google.gdata.data.appsforyourdomain.provisioning.UserEntry
import com.google.gdata.data.appsforyourdomain.provisioning.UserFeed

import com.lunatech.openconnect.Authenticate

import reactivemongo.api.gridfs.GridFS
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import reactivemongo.api._
import models._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import org.joda.time.DateTime


object Application extends Controller with MongoController with Secured {

  val GOOGLE_URL = "https://accounts.google.com/o/oauth2/auth"



  def expenses = db.collection[BSONCollection]("expenses")
  def recurringExpenses = db.collection[BSONCollection]("recurringexpenses")

  val gridFS = new GridFS(db)

  gridFS.ensureIndex().onComplete {
    case index =>
      Logger.info(s"Checked index, result is $index")
  }

  def index = IsAuthenticated { (username, name) => implicit request =>

  	Redirect(routes.Application.expensesIndexPerYear(new DateTime().getYear))
  }

  def adminIndex = IsAuthenticated { (username, name) => implicit request =>

  	Redirect(routes.Application.adminIndexPerYear(new DateTime().getYear))
  }

  def expensesIndexPerYear(year: Int) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val query = BSONDocument(
        "$query" -> BSONDocument("email" -> username, "year" -> year),
        "$orderby" -> BSONDocument("year" -> -1))

      val found = expenses.find(query).cursor[Expense]
      found.toList().map { expenses =>
        Ok(views.html.expensesindex(username, expenses, year))
      }
    }
  }

  // TODO: only the admins can view the expenses to review
  def adminIndexPerYear(year: Int) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val ids = Seq("submitted", "approved", "rejected")
      val queryA = BSONDocument("status" -> BSONDocument("$in" -> ids))
      val query = BSONDocument(
        "$query" -> queryA,
        "$orderby" -> BSONDocument("start_date" -> -1))
      val found = expenses.find(query).cursor[Expense]
      found.toList().map { expenses =>
        Ok(views.html.reviewindex(username, expenses, year))
      }
    }
  }

 // TODO: only the admins can review an expense
 def review(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      for {
        maybeExpense <- futureExpense
        result <- maybeExpense.map { expense =>
          import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
          gridFS.find(BSONDocument(
             "$query" -> BSONDocument("expenses" -> expense.id.get),
             "$orderby" -> BSONDocument("uploadDate" -> 1))
            ).toList().map { files =>
            val filesWithId = files.map { file =>
              file.id.asInstanceOf[BSONObjectID].stringify -> file
            }
            Ok(views.html.reviewform(username, name, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.statusDetails, expense.items, expense.comments, Some(filesWithId)))
          }
        }.getOrElse(Future(NotFound))
      } yield result
    }
  }


  // TODO: Check that the user is allow to see the expenses. Only the author and the reviewer can see the expense.
  def expensesShow(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      for {
        maybeExpense <- futureExpense
        result <- maybeExpense.map { expense =>
          import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
          gridFS.find(BSONDocument(
             "$query" -> BSONDocument("expenses" -> expense.id.get),
             "$orderby" -> BSONDocument("uploadDate" -> 1))
            ).toList().map { files =>
            val filesWithId = files.map { file =>
              file.id.asInstanceOf[BSONObjectID].stringify -> file
            }
            Ok(views.html.expensesform(username, name, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.statusDetails, expense.items, expense.comments, Some(filesWithId)))
          }
        }.getOrElse(Future(NotFound))
      } yield result
    }
  }


  val recurringForm = Form[RecurringExpense] (
    mapping (
          "id" -> optional(of[String] verifying pattern(
          """[a-fA-F0-9]{24}""".r,
          "constraint.objectId",
          "error.objectId")),
       "description" -> nonEmptyText,
       "amount" -> of[Double],
       "frequence" -> nonEmptyText,
       "author" -> nonEmptyText
    ) {
      (id, description, amount, frequency, author) =>
        RecurringExpense(id.map(new BSONObjectID(_)), description, amount, frequency, author)
    } {
      expense => Some(expense.id.map(_.stringify), expense.description, expense.amount, expense.frequence, expense.author)
    }
  )


  val commentForm = Form[Comment] (
    mapping (
       "id" -> optional(text),
       "author" -> nonEmptyText,
        "email" -> email,
        "content" -> nonEmptyText
    ) {
      (id, author, email, content) =>
        Comment(id, author, email, new DateTime(), content)
    } {
      comment => Some(comment.id, comment.author, comment.email, comment.content)
    }
  )

  val expenseForm = Form[Expense] (
    mapping (
          "id" -> optional(of[String] verifying pattern(
          """[a-fA-F0-9]{24}""".r,
          "constraint.objectId",
          "error.objectId")),
          "status" -> nonEmptyText,
          "reference" -> optional(text),
          "author" -> nonEmptyText,
          "email" -> nonEmptyText,
          "startDate" -> of[Long],
          "endDate" -> of[Long],
          "items" -> list[Item]  (
             mapping(
              "date" -> of[Long],
              "name" -> nonEmptyText,
              "amount" -> of[Double],
              "note" -> optional(text)) { (date, name, amount, note) =>
              Item(
                 new DateTime(date),
                name,
                amount,
                note
               )
          } { item => {
            Some(
              (item.date.getMillis,
                item.name,
                item.amount,
                item.note
               ))
          }
          }
          )
        )
        { (id, status, reference, author, email, startDate, endDate, items) =>
          Expense(
            id.map(new BSONObjectID(_)),
            status,
            reference,
            author,
            email,
            new DateTime(startDate),
            new DateTime(endDate),
            items)

        } { expense => {
          Some(
            (expense.id.map(_.stringify),
              expense.status,
              expense.reference,
              expense.author,
              expense.email,
              expense.startDate.getMillis,
              expense.endDate.getMillis,
              expense.items.toList))
        }
      }
   )

  def expensesNew() = IsAuthenticated { (username, name)  => implicit request =>
    expenseForm.bindFromRequest.fold(
      errors => {
        // TODO: Format the dates and extract the items
        BadRequest(views.html.expensesform(username, name, new DateTime(), new DateTime(), errors, Seq(), Seq(), Seq()))
      },
      expense => Async {
        import models.Expense.ItemBSONWriter
        val doc = BSON.write(expense)
        val id = doc.getAs[BSONObjectID]("_id")
        expenses.insert(doc).map{ lastError =>
          Redirect(routes.Application.expensesShow(id.get.stringify)).flashing("success" -> "Your expense has been created")
        }
      })
  }

 // TODO: only the owner can delete the expense
 def expensesDelete(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     Async {
        val objectId = new BSONObjectID(id)
        val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]

        futureExpense.flatMap { expense =>

          gridFS.find(BSONDocument("expenses" -> new BSONObjectID(id))).toList().flatMap { files =>
              val deletions = files.map { file =>
                gridFS.remove(file)
              }
              Future.sequence(deletions)
            }.flatMap { _ =>
              expenses.remove(BSONDocument("_id" -> new BSONObjectID(id)))
            }.map(_ => Redirect(routes.Application.expensesIndexPerYear(expense.get.startDate.getYear)).flashing("success" -> "Expense has been deleted")).recover { case _ => InternalServerError }
          }
        }
      }


  // TODO: only the owner can edit the expense
  def expensesEdit(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     expenseForm.bindFromRequest.fold(
      errors => AsyncResult {
        val objectId = new BSONObjectID(id)
        val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
        futureExpense.map { expense =>
          BadRequest(views.html.expensesform(username, name, expense.get.startDate, expense.get.endDate, errors, expense.get.statusDetails, expense.get.items, expense.get.comments))
        }
      },
      expense => AsyncResult {
        val objectId = new BSONObjectID(id)
        import models.Expense.ItemBSONWriter

        val modifier = BSONDocument(
          "$set" -> BSONDocument(
            "start_date" -> BSONDateTime(expense.startDate.getMillis),
            "end_date" -> BSONDateTime(expense.endDate.getMillis),
            "reference" -> BSONString(expense.reference.getOrElse("")),
            "items" -> expense.items))
        // ok, let's do the update
        expenses.update(BSONDocument("_id" -> objectId), modifier).map { _ =>
          Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Expense has been updated")
        }
      })
  }

  def addComment(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     import models.Expense.CommentBSONWriter

     commentForm.bindFromRequest.fold(
      errors => Async {
        val objectId = new BSONObjectID(id)
        val futureExpense = expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
        futureExpense.map { expense =>
          BadRequest(views.html.expensesform(username, name, expense.get.startDate, expense.get.endDate, expenseForm, expense.get.statusDetails, expense.get.items, expense.get.comments))
        }
      },
      comment => Async {
        val objectId = new BSONObjectID(id)
        val futureExpense = expenses.find(BSONDocument("_id" -> objectId)).one[Expense]

        futureExpense.flatMap { expense =>
          val comments = expense.get.comments :+ comment
          val modifier = BSONDocument(
            "$set" -> BSONDocument(
              "comments" -> comments))
          expenses.update(BSONDocument("_id" -> objectId), modifier).map { _ =>
            // Send a comment to the right user
            if (comment.author == expense.get.author) {
              // Send to admin
              sendCommentEmailToAdmins(comment.author, comment.email, expense.get, comment)
            } else {
              sendCommentEmailToUser(comment.author, comment.email, expense.get, comment)
            }

            Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Your comment has been added.")
          }
        }
      })
  }

  // TODO: only the owner can delete the comment
  def deleteComment(id: String, commentId: String) = IsAuthenticated { (username, name)  => implicit request =>
    import models.Expense.CommentBSONWriter

    Async {
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        val comments = expense.get.comments.filterNot(_.id.get == commentId)
        val modifier = BSONDocument(
          "$set" -> BSONDocument(
            "comments" -> comments))
        expenses.update(BSONDocument("_id" -> objectId), modifier).map { _ =>
          Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Comment has been deleted.")
        }
      }
    }
  }

  def submitExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val result = convertTo(id, "submitted", name, Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Expense has been submitted."))
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendSubmittedEmail(expense.get)
        result
      }

    }
  }

  // TODO: only admin can do that
  def approveExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      var result = convertTo(id, "approved", name, Redirect(routes.Application.review(id)).flashing("success" -> "Expense has been approved."))
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendApprovedEmail(expense.get)
        result
      }
    }
  }

  // TODO: only admin can do that
  def  rejectExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      var result = convertTo(id, "rejected", name, Redirect(routes.Application.review(id)).flashing("success" -> "Expense has been rejected."))
      val objectId = new BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendRejectedEmail(expense.get)
        result
      }
    }
  }

  // This should be a private function that is called by specialized function
  private def convertTo(id: String, status: String, name: String, action: Result): Future[Result] = {
    import models.Expense.StatusDetailsWriter

    val objectId = new BSONObjectID(id)
    val futureExpense = expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
    futureExpense.flatMap { expense =>
      val statusDetails = expense.get.statusDetails :+ StatusDetails(new DateTime(), name, status)
      val modifier = BSONDocument(
        "$set" -> BSONDocument(
          "status" -> BSONString(status),
          "status_details" -> statusDetails))
      expenses.update(BSONDocument("_id" -> objectId), modifier).map { _ =>
        action
      }
    }
  }

 def expensesNewForm = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      // Do we need to include a monthly recurring expense?
      val queryRecurringMonthly = BSONDocument(
        "$query" -> BSONDocument("author" -> username, "frequence" -> "Every month"))

      val query = BSONDocument(
        "$query" -> BSONDocument("email" -> username, "year" -> new DateTime().getYear),
        "$orderby" -> BSONDocument("year" -> -1))

      for { recurring <- recurringExpenses.find(queryRecurringMonthly).cursor[RecurringExpense].toList()
            previous <- expenses.find(query).one[Expense]
        } yield {
          // Add the recurring items
          val monthlyItems = recurring.map { recurring =>
              Item(new DateTime(), recurring.description, recurring.amount, Option("Monthly payment"))
          }
          val (startDate, endDate) = previous match {
            case Some(ex) => (previous.get.endDate.plusDays(1), previous.get.endDate.plusMonths(1))
            case None => (new DateTime(), new DateTime().plusMonths(1))
          }
          val expense = new Expense (
                None,
                "draft",
                None,
                name,
                username,
                startDate,
                endDate,
                monthlyItems)
         Ok(views.html.expensesnew(username, name, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.items, expense.comments))
      }
   }
  }

  def recurringShow(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      val objectId = new BSONObjectID(id)
      val futureExpense = recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]

      futureExpense.map { expense =>
          Ok(views.html.recurringform(username, name, recurringForm.fill(expense.get)))
      }
    }
  }

  // TODO: only the owner can edit the expense
 def recurringEdit(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     recurringForm.bindFromRequest.fold(
      errors => AsyncResult {
        val objectId = new BSONObjectID(id)
        val futureExpense= recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]
        futureExpense.map { expense =>
          BadRequest(views.html.recurringnew(username, name, errors))
        }
      },
      expense => AsyncResult {
        val objectId = new BSONObjectID(id)
        import models.RecurringExpense.RecurringExpenseBSONWriter

        val modifier = BSONDocument(
          "$set" -> BSONDocument(
            "description" -> BSONString(expense.description),
            "amount" -> BSONDouble(expense.amount),
            "frequence" -> BSONString(expense.frequence)))
        // ok, let's do the update
        recurringExpenses.update(BSONDocument("_id" -> objectId), modifier).map { _ =>
          Redirect(routes.Application.recurringShow(id)).flashing("success" -> "Recurring expense has been updated")
        }
      })
  }

  def recurringNewForm = IsAuthenticated { (username, name)  => implicit request =>

     Ok(views.html.recurringnew(username, name, recurringForm))
  }

  def recurringNew = IsAuthenticated { (username, name)  => implicit request =>

     recurringForm.bindFromRequest.fold(
      errors => {
        BadRequest(views.html.recurringnew(username, name, errors))
      },
      expense => Async {
        import models.Expense.ItemBSONWriter
        val doc = BSON.write(expense)
        val id = doc.getAs[BSONObjectID]("_id")
        recurringExpenses.insert(doc).map { lastError =>
          Redirect(routes.Application.recurringShow(id.get.stringify)).flashing("success" -> "Your recurring expense has been created")
        }
      })
  }

  def recurringIndex = IsAuthenticated { (username, name)  => implicit request =>
     Async {
      // TODO: Sort per date and filter per year
      val query = BSONDocument(
        "$query" -> BSONDocument("author" -> username),
        "$orderby" -> BSONDocument("desciption" -> -1))

      val found = recurringExpenses.find(query).cursor[RecurringExpense]
      found.toList().map { expenses =>
        Ok(views.html.recurringindex(username, expenses))
      }
    }
  }

  // TODO: only the owner can delete the expense
  def recurringDelete(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     Async {
        val objectId = new BSONObjectID(id)
        val futureExpense = recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]

        futureExpense.flatMap { expense =>
          recurringExpenses.remove(BSONDocument("_id" -> objectId)).map { lastError =>
            Redirect(routes.Application.recurringIndex()).flashing("success" -> "Recurring expense has been deleted")
          }
        }
    }
  }

  // -- Emails

  private def sendCommentEmailToAdmins(username: String, email: String, expense: Expense, comment: Comment) = { implicit request: RequestHeader =>
    import com.typesafe.plugin._
    import play.api.Play.current
    val mail = use[MailerPlugin].email
    mail.setSubject(username + " left a commment - expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") from " + expense.author )
    mail.setRecipient(Play.configuration.getString("email.recipient").get.split(",").toList:_*)
    mail.setFrom(expense.author + " <" + expense.email + ">")
    val template = views.html.emails.notifycommenttoadmin.render(expense, comment, request)

    // sends html
    mail.sendHtml(template.body)
  }


  private def sendCommentEmailToUser(username: String, email: String, expense: Expense, comment: Comment) = { implicit request: RequestHeader =>
    import com.typesafe.plugin._
    import play.api.Play.current
    val mail = use[MailerPlugin].email
    mail.setSubject(username + " left a commment - expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ")")
    mail.setRecipient(expense.author + " <" + expense.email + ">")
    mail.setFrom(username + "<" + email + ">")
    val template = views.html.emails.notifycommenttouser.render(username, expense, comment, request)

    // sends html
    mail.sendHtml(template.body)
  }


  private def sendRejectedEmail(expense: Expense) = { implicit request: RequestHeader =>
    import com.typesafe.plugin._
    import play.api.Play.current
    val mail = use[MailerPlugin].email
    val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
    mail.setSubject("You expenses expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") have been rejected.")
    mail.setFrom(Play.configuration.getString("email.from").get)
    mail.setRecipient(expense.author + " <" + expense.email + ">")
    val template = views.html.emails.notifyrefusedexpense.render(expense, request)

    // sends html
    mail.sendHtml(template.body)
  }

  private def sendSubmittedEmail(expense: Expense) = { implicit request: RequestHeader =>
    import com.typesafe.plugin._
    import play.api.Play.current
    val mail = use[MailerPlugin].email
    val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
    mail.setSubject(expense.author + " submitted an expense. Please review it.")
    mail.setFrom(expense.author + " <" + expense.email + ">")
    mail.setRecipient(Play.configuration.getString("email.recipient").get.split(",").toList:_*)
    val template = views.html.emails.notifynewexpense.render(expense, request)

    // sends html
    mail.sendHtml(template.body)
  }


  private def sendApprovedEmail(expense: Expense) = { implicit request: RequestHeader =>
    import com.typesafe.plugin._
    import play.api.Play.current
    val mail = use[MailerPlugin].email
    val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
    mail.setSubject("You expenses expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") have been approved.")
    mail.setFrom(Play.configuration.getString("email.from").get)
    mail.setRecipient(expense.author + " <" + expense.email + ">")
    val template = views.html.emails.notifyapprovedexpense.render(expense, request)

    // sends html
    mail.sendHtml(template.body)
  }


  // -- Attachments

  // TODO: This should be secured ie user have to be registered
  def saveAttachments(id: String) = Action(gridFSBodyParser(gridFS))  {  implicit request =>
    val futureFile = request.body.files.head.ref
    val futureUpdate = for {
      file <- futureFile
      updateResult <- {
        gridFS.files.update(
          BSONDocument("_id" -> file.id),
          BSONDocument("$set" -> BSONDocument("expenses" -> BSONObjectID(id))))
      }
    } yield updateResult

    Async {
      futureUpdate.map {
        case _ => Ok
      }.recover {
        case e => InternalServerError(e.getMessage())
      }
    }
  }

  // TODO: only the owner or admin can save the file
  def getAttachment(id: String) = IsAuthenticated { (username, name)  => request =>
    Async {
      val file = gridFS.find(BSONDocument("_id" -> new BSONObjectID(id)))
      serve(gridFS, file)
    }
  }

  // TODO: only the owner can delete the file
  def deleteAttachment(id: String) = IsAuthenticated { (username, name)  => implicit request =>
    Async {
      gridFS.remove(new BSONObjectID(id)).map(_ => Ok).recover { case _ => InternalServerError }
    }
  }


  // -- Authentication

  /**
   * Login page.
   */

     /**
      * Login page.
      */
     def login = Action { implicit request =>
       if(Play.isProd) {
         val clientId: String = Play.configuration.getString("google.clientId").get
         val state: String = new BigInteger(130, new SecureRandom()).toString(32)

         Ok(views.html.login(clientId)).withSession("state" -> state)
       } else {
         Redirect(routes.Application.index).withSession("email" -> "nicolas.leroux@lunatech.com")
       }
     }

     def authenticate(code: String, id_token: String, access_token: String) = Action { implicit request =>

     Async {
       val response = Authenticate.authenticateToken(code, id_token, access_token)

       response.map {
           case Left(parameters) => Redirect(routes.Application.index).withSession(parameters.toArray: _*)
           case Right(message) => Redirect(routes.Application.login).withNewSession.flashing("error" -> message.toString())
         }
       }
   }


 /**
   * Logout and clean the session.
   */
  def logout = Action {
    Redirect(routes.Application.login).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }

}

object Secure {

  def isAdministrator(email: String) = {
    import play.api.Play.current
    Play.configuration.getString("email.recipient").getOrElse("").contains(email)
  }

}

/**
 * Provide security features
 */
trait Secured {

  /**
   * Retrieve the connected user email.
   */
   private def username(request: RequestHeader):Option[(String, String)] = {
          request.session.get("email") match {
          case Some(email) => Option((email, email/*request.session.get("firstname").getOrElse("") + " " + request.session.get("lastname").getOrElse("")*/))
            case _ => None
         }
      }
  /**
   * Redirect to login if the user in not authorized.
   */
  private def onUnauthorized(request: RequestHeader) = {
    Results.Redirect(routes.Application.login).withSession("originalUrl" -> request.uri)
  }

  // --

  /**
 * Action for authenticated users.
 */
 def IsAuthenticated(f: => (String, String) => Request[AnyContent] => Result) =
   Security.Authenticated(username, onUnauthorized) { case (username, name) =>
     Action(request => f(username, name)(request))
 }



  def isOnWhiteList(email:String) = {
    import play.api.Play.current
    val CONSUMER_KEY = Play.configuration.getString("google.key")
    val CONSUMER_SECRET =  Play.configuration.getString("google.secret")
    val DOMAIN =  Play.configuration.getString("google.domain")

    val oauthParameters = new GoogleOAuthParameters()
    oauthParameters.setOAuthConsumerKey(CONSUMER_KEY.get)
    oauthParameters.setOAuthConsumerSecret(CONSUMER_SECRET.get)
    oauthParameters.setOAuthType(OAuthType.TWO_LEGGED_OAUTH)
    val signer = new OAuthHmacSha1Signer()
    val feedUrl = new URL("https://apps-apis.google.com/a/feeds/" + DOMAIN.get + "/user/2.0")

    val service = new UserService("ProvisiongApiClient")
    service.setOAuthCredentials(oauthParameters, signer)
    service.useSsl()
    val resultFeed = service.getFeed(feedUrl,  classOf[UserFeed])

    import scala.collection.JavaConversions._
    val users =  resultFeed.getEntries.toSet
    val filteredUsers = users.map( entry => entry.getTitle().getPlainText() + "@" + DOMAIN.get)
    Logger.info(filteredUsers.mkString)
    filteredUsers.contains(email)
  }

}
