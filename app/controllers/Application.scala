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


import java.io.IOException
import java.math.BigInteger
import java.security.SecureRandom

import reactivemongo.api.gridfs.{ // ReactiveMongo GridFS
  DefaultFileToSave, FileToSave, GridFS, ReadFile
}
import reactivemongo.api.gridfs.Implicits._
import play.modules.reactivemongo.json._
import play.modules.reactivemongo.json.collection._

import MongoController.readFileReads


import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson._
import reactivemongo.api._
import models._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import org.joda.time.DateTime
import play.api.Play.current

import play.libs.mailer._

import reactivemongo.bson.BSONDateTime
import play.api.libs.json.{ JsError, JsResult, JsSuccess }
import play.api.libs.functional.syntax._
import play.modules.reactivemongo.json.BSONFormats, BSONFormats.{ BSONDateTimeFormat, BSONDocumentFormat }



object Application extends Controller with MongoController with Secured {

  val GOOGLE_URL = "https://accounts.google.com/o/oauth2/auth"
  type JSONReadFile = ReadFile[JSONSerializationPack.type, JsString]

  def expenses = db.collection[BSONCollection]("expenses")
  def recurringExpenses = db.collection[BSONCollection]("recurringexpenses")

  val gridFS = new GridFS(db)


  gridFS.ensureIndex().onComplete {
    case index =>
      Logger.info(s"Checked index, result is $index")
  }

  def index = IsAuthenticated { (username, name) => implicit request =>

  	Future(Redirect(routes.Application.expensesIndexPerYear(new DateTime().getYear)))
  }

  def adminIndex = IsAuthenticated { (username, name) => implicit request =>

  	Future(Redirect(routes.Application.adminIndexPerYear(new DateTime().getYear)))
  }

  def expensesIndexPerYear(year: Int) = IsAuthenticated { (username, name)  => implicit request =>
      val query = BSONDocument(
        "$query" -> BSONDocument("email" -> username, "year" -> year),
        "$orderby" -> BSONDocument("year" -> -1))

      val found = expenses.find(query).cursor[Expense]
      found.collect[List]().map { expenses =>
        Ok(views.html.expensesindex(username, expenses, year))
      }
    }


  // TODO: only the admins can view the expenses to review
  def adminIndexPerYear(year: Int) = IsAuthenticated { (username, name)  => implicit request =>
      val ids = Seq("submitted", "approved", "rejected")
      val queryA = BSONDocument("status" -> BSONDocument("$in" -> ids), "year" -> year)
      val query = BSONDocument(
        "$query" -> queryA,
        "$orderby" -> BSONDocument("submit_date" -> -1, "_id" -> -1))
      val found = expenses.find(query).cursor[Expense]
      found.collect[List]().map { expenses =>
        Ok(views.html.reviewindex(username, expenses, year))
      }
    }


 // TODO: only the admins can review an expense
 def review(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      val objectId = BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      for {
        maybeExpense <- futureExpense
        result <- maybeExpense.map { expense =>
          gridFS.find[JsObject, JSONReadFile](Json.obj(
             "expenses" -> id)
           ).collect[List]().map { files =>
              val filesWithId = files.map { file =>
              file.id.value -> file
            }
            Ok(views.html.reviewform(username, name, expense.submitDate, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.statusDetails, expense.items, expense.comments, Some(filesWithId)))
          }
        }.getOrElse(Future(NotFound))
      } yield result
    }


  // TODO: Check that the user is allow to see the expenses. Only the author and the reviewer can see the expense.
  def expensesShow(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      val objectId = BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      for {
        maybeExpense <- futureExpense
        result <- maybeExpense.map { expense =>
          gridFS.find[JsObject, JSONReadFile](Json.obj(
             "expenses" -> id)
           ).collect[List]().map { files =>
              val filesWithId = files.map { file =>
              file.id.value -> file
            }
            Ok(views.html.expensesform(username, name, expense.submitDate, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.statusDetails, expense.items, expense.comments, Some(filesWithId)))
          }
        }.getOrElse(Future(NotFound))
      } yield result
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
        RecurringExpense(id.map(BSONObjectID(_)), description, amount, frequency, author)
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
          "submitDate" -> of[Long],
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
        { (id, submitDate, status, reference, author, email, startDate, endDate, items) =>
          Expense(
            id.map(BSONObjectID(_)),
            new DateTime(),
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
              expense.submitDate.getMillis,
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
        Future(BadRequest(views.html.expensesform(username, name, new DateTime(), new DateTime(), new DateTime(), errors, Seq(), Seq(), Seq())))
      },
      expense => {
        import models.Expense.ItemBSONWriter
        val doc = BSON.write(expense.withDateRange)
        val id = doc.getAs[BSONObjectID]("_id")
        expenses.insert(doc).map{ lastError =>
          Redirect(routes.Application.expensesShow(id.get.stringify)).flashing("success" -> "Your expense has been created")
        }
      })
  }

 // TODO: only the owner can delete the expense
 def expensesDelete(id: String) = IsAuthenticated { (username, name)  => implicit request =>
        val objectId = BSONObjectID(id)
        val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]

        futureExpense.flatMap { expense =>

          gridFS.find[JsObject, JSONReadFile](Json.obj("expenses" -> id)).collect[List]().flatMap { files =>
              val deletions = files.map { file =>
                gridFS.remove(file)
              }
              Future.sequence(deletions)
            }.flatMap { _ =>
              expenses.remove(BSONDocument("_id" -> BSONObjectID(id)))
            }.map(_ => Redirect(routes.Application.expensesIndexPerYear(expense.get.startDate.getYear)).flashing("success" -> "Expense has been deleted")).recover { case _ => InternalServerError }
          }
        }



  // TODO: only the owner can edit the expense
  def expensesEdit(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     expenseForm.bindFromRequest.fold(
      errors => {
        val objectId = BSONObjectID(id)
        val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
        futureExpense.map { expense =>
          BadRequest(views.html.expensesform(username, name, expense.get.submitDate, expense.get.startDate, expense.get.endDate, errors, expense.get.statusDetails, expense.get.items, expense.get.comments))
        }
      },
      expense => {
        val objectId = BSONObjectID(id)
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
      errors => {
        val objectId = BSONObjectID(id)
        val futureExpense = expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
        futureExpense.map { expense =>
          BadRequest(views.html.expensesform(username, name, expense.get.submitDate, expense.get.startDate, expense.get.endDate, expenseForm, expense.get.statusDetails, expense.get.items, expense.get.comments))
        }
      },
      comment => {
        val objectId = BSONObjectID(id)
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
              sendCommentEmailToAdmins(comment.author, comment.email, expense.get, comment, request)
            } else {
              sendCommentEmailToUser(comment.author, comment.email, expense.get, comment, request)
            }

            Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Your comment has been added.")
          }
        }
      })
  }

  // TODO: only the owner can delete the comment
  def deleteComment(id: String, commentId: String) = IsAuthenticated { (username, name)  => implicit request =>
    import models.Expense.CommentBSONWriter

      val objectId = BSONObjectID(id)
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


  def submitExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      val result = convertTo(id, "submitted", name, Redirect(routes.Application.expensesShow(id)).flashing("success" -> "Expense has been submitted."))
      val objectId = BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendSubmittedEmail(expense.get, request)
        result
      }

    }


  // TODO: only admin can do that
  def approveExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      var result = convertTo(id, "approved", name, Redirect(routes.Application.review(id)).flashing("success" -> "Expense has been approved."))
      val objectId = BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendApprovedEmail(expense.get, request)
        result
      }
    }

  // TODO: only admin can do that
  def  rejectExpense(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      var result = convertTo(id, "rejected", name, Redirect(routes.Application.review(id)).flashing("success" -> "Expense has been rejected."))
      val objectId = BSONObjectID(id)
      val futureExpense= expenses.find(BSONDocument("_id" -> objectId)).one[Expense]
      futureExpense.flatMap { expense =>
        sendRejectedEmail(expense.get, request)
        result
      }
    }

  // This should be a private function that is called by specialized function
  private def convertTo(id: String, status: String, name: String, action: Result): Future[Result] = {
    import models.Expense.StatusDetailsWriter

    val objectId = BSONObjectID(id)
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
      // Do we need to include a monthly recurring expense?
      val queryRecurringMonthly = BSONDocument(
        "$query" -> BSONDocument("author" -> username, "frequence" -> "Every month"))

      val query = BSONDocument(
        "$query" -> BSONDocument("email" -> username, "year" -> new DateTime().getYear),
        "$orderby" -> BSONDocument("year" -> -1))

      for { recurring <- recurringExpenses.find(queryRecurringMonthly).cursor[RecurringExpense].collect[List]()
            previous <- expenses.find(query).one[Expense]
        } yield {
          // Add the recurring items
          val monthlyItems = recurring.map { recurring =>
              Item(new DateTime(), recurring.description, recurring.amount, Option("Monthly payment"))
          }
        /*  val (startDate, endDate) = previous match {
            case Some(ex) => (previous.get.endDate.plusDays(0), previous.get.endDate.plusMonths(0))
            case None => (new DateTime(), new DateTime().plusMonths(1))
          } */
          val expense = new Expense (
                None,
                new DateTime(),
                "draft",
                None,
                name,
                username,
                new DateTime(),
                new DateTime(),
                monthlyItems)
         Ok(views.html.expensesnew(username, name, expense.startDate, expense.endDate, expenseForm.fill(expense), expense.items, expense.comments))
      }
   }


  def recurringShow(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      val objectId = BSONObjectID(id)
      val futureExpense = recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]

      futureExpense.map { expense =>
          Ok(views.html.recurringform(username, name, recurringForm.fill(expense.get)))
      }
    }


  // TODO: only the owner can edit the expense
 def recurringEdit(id: String) = IsAuthenticated { (username, name)  => implicit request =>
     recurringForm.bindFromRequest.fold(
      errors => {
        val objectId = BSONObjectID(id)
        val futureExpense= recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]
        futureExpense.map { expense =>
          BadRequest(views.html.recurringnew(username, name, errors))
        }
      },
      expense => {
        val objectId = BSONObjectID(id)
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

     Future(Ok(views.html.recurringnew(username, name, recurringForm)))
  }

  def recurringNew = IsAuthenticated { (username, name)  => implicit request =>

     recurringForm.bindFromRequest.fold(
      errors => {
        Future(BadRequest(views.html.recurringnew(username, name, errors)))
      },
      expense => {
        import models.Expense.ItemBSONWriter
        val doc = BSON.write(expense)
        val id = doc.getAs[BSONObjectID]("_id")
        recurringExpenses.insert(doc).map { lastError =>
          Redirect(routes.Application.recurringShow(id.get.stringify)).flashing("success" -> "Your recurring expense has been created")
        }
      })
  }

  def recurringIndex = IsAuthenticated { (username, name)  => implicit request =>
      // TODO: Sort per date and filter per year
      val query = BSONDocument(
        "$query" -> BSONDocument("author" -> username),
        "$orderby" -> BSONDocument("desciption" -> -1))

      val found = recurringExpenses.find(query).cursor[RecurringExpense]
      found.collect[List]().map { expenses =>
        Ok(views.html.recurringindex(username, expenses))
      }
    }


  // TODO: only the owner can delete the expense
  def recurringDelete(id: String) = IsAuthenticated { (username, name)  => implicit request =>
        val objectId = BSONObjectID(id)
        val futureExpense = recurringExpenses.find(BSONDocument("_id" -> objectId)).one[RecurringExpense]

        futureExpense.flatMap { expense =>
          recurringExpenses.remove(BSONDocument("_id" -> objectId)).map { lastError =>
            Redirect(routes.Application.recurringIndex()).flashing("success" -> "Recurring expense has been deleted")
          }
        }
    }


  // -- Emails

  private def sendCommentEmailToAdmins(username: String, email: String, expense: Expense, comment: Comment, request: RequestHeader) = {
    try {
      val mail = new Email()
      mail.setSubject(username + " left a commment - expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") from " + expense.author)
      Play.configuration.getString("email.recipient").get.split(",").map { x =>
        mail.addTo(x)
      }
      mail.setFrom(expense.email)
      val template = views.html.emails.notifycommenttoadmin.render(expense, comment, request)

      // sends html
      mail.setBodyHtml(template.body)
      MailerPlugin.send(mail)
    } catch {
      case e:Throwable => Logger.error("Error sending email " + e)
    }
  }


  private def sendCommentEmailToUser(username: String, email: String, expense: Expense, comment: Comment, request: RequestHeader) = {
    try {
      val mail = new Email()
      mail.setSubject(username + " left a commment - expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ")")
      mail.addTo(expense.email)
      mail.setFrom(email)
      val template = views.html.emails.notifycommenttouser.render(username, expense, comment, request)

      // sends html
      mail.setBodyHtml(template.body)
      MailerPlugin.send(mail)
    } catch {
      case e:Throwable => Logger.error("Error sending email " + e)
    }
  }


  private def sendRejectedEmail(expense: Expense, request: RequestHeader) = {
    try {
      val mail = new Email()
      val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
      mail.setSubject("You expenses expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") have been rejected.")
      mail.setFrom(Play.configuration.getString("email.from").get)
      mail.addTo(expense.email)
      val template = views.html.emails.notifyrefusedexpense.render(expense, request)

      // sends html
      mail.setBodyHtml(template.body)
      MailerPlugin.send(mail)
    } catch {
      case e:Throwable => Logger.error("Error sending email " + e)
    }
  }

  private def sendSubmittedEmail(expense: Expense, request: RequestHeader) = {
    try {
      val mail = new Email()
      val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
      mail.setSubject(expense.author + " submitted an expense. Please review it.")
      mail.setFrom(expense.email)
      Play.configuration.getString("email.recipient").get.split(",").map { x =>
        mail.addTo(x)
      }
      val template = views.html.emails.notifynewexpense.render(expense, request)

      // sends html
      mail.setBodyHtml(template.body)
      MailerPlugin.send(mail)
    } catch {
      case e:Throwable => Logger.error("Error sending email " + e)
    }
  }


  private def sendApprovedEmail(expense: Expense, request: RequestHeader) = {
    try {
      val mail = new Email()
      val fmt = new java.text.SimpleDateFormat(" MMM yyyy")
      mail.setSubject("You expenses expense (" + Time.ordinal(expense.startDate) + expense.startDate.toString(" MMM yyyy") + " - " + Time.ordinal(expense.endDate) + expense.endDate.toString(" MMM yyyy") + ") have been approved.")
      mail.setFrom(Play.configuration.getString("email.from").getOrElse("expenses@lunatech.fr"))
      mail.addTo(expense.email)
      mail.addCc(Play.configuration.getString("email.cc").getOrElse("billing-fr@lunatech.fr"))
      val template = views.html.emails.notifyapprovedexpense.render(expense, request)

      // sends html
      mail.setBodyHtml(template.body)
      MailerPlugin.send(mail)
    } catch {
      case e:Throwable => Logger.error("Error sending email " + e)
    }
  }


  // -- Attachments

  // TODO: This should be secured ie user have to be registered
  def saveAttachments(id: String) = Action.async(gridFSBodyParser(gridFS))  {  implicit request =>
    val futureFile = request.body.files.head.ref
    val futureUpdate = for {
      file <- futureFile
      updateResult <- {
        gridFS.files.update(
          Json.obj("_id" -> file.id),
          Json.obj("$set" -> Json.obj("expenses" -> id)))
      }
    } yield updateResult

      futureUpdate.map {
        case _ => Ok
      }.recover {
        case e => InternalServerError(e.getMessage())
      }
    }


  // TODO: only the owner or admin can save the file
  def getAttachment(id: String) = IsAuthenticated { (username, name)  => request =>
      //val file = gridFS.find[BSONDocument, JSONReadFile](BSONDocument("_id" -> BSONObjectID(id)))
      val file = gridFS.find[JsObject, JSONReadFile](Json.obj("_id" -> id))
      serve[JsString, JSONReadFile](gridFS)(file)
    }


  // TODO: only the owner can delete the file
  def deleteAttachment(id: String) = IsAuthenticated { (username, name)  => implicit request =>
      gridFS.remove(Json toJson id).map(_ => Ok).recover { case _ => InternalServerError }
  }



  // -- Authentication

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

     def authenticate(code: String, id_token: String, access_token: String) = Action.async { implicit request =>

       val response = Authenticate.authenticateToken(code, id_token, access_token)

       response.map {
           case Left(tokenInfo) => Redirect(routes.Application.index).withSession(Seq("email" -> tokenInfo.getEmail).toArray: _*)
           case Right(message) => {
             Redirect(routes.Application.login).withNewSession.flashing("error" -> message.toString())
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

object Authenticate {

  val GOOGLE_CLIENTID = "google.clientId"
  val GOOGLE_DOMAIN = "google.domain"
  val GOOGLE_SECRET = "google.secret"

  val GOOGLE_CONF = "https://accounts.google.com/.well-known/openid-configuration"
  val REVOKE_ENDPOINT = "revocation_endpoint"

  val ERROR_GENERIC: String = "Something went wrong, please try again later"

  def generateState: String = new BigInteger(130, new SecureRandom()).toString(32)

  /**
   * Accepts an authResult['code'], authResult['id_token'], and authResult['access_token'] as supplied by Google.
   * Returns authentication email and token parameters if successful, otherwise revokes user-granted permissions and returns an error.
   */
  def authenticateToken(code: String, id_token: String, accessToken: String): Future[Either[Tokeninfo, AuthenticationError]] = {

    val clientId: String = Play.configuration.getString("google.clientId").get
    val secret: String = Play.configuration.getString("google.secret").get

    try {

      val transport: NetHttpTransport = new NetHttpTransport()
      val jsonFactory: JacksonFactory = new JacksonFactory()

      val tokenResponse: GoogleTokenResponse = new GoogleAuthorizationCodeTokenRequest(
        transport, jsonFactory, clientId, secret, code, "postmessage"
      ).execute()

      val credential: GoogleCredential = new GoogleCredential.Builder()
        .setJsonFactory(jsonFactory)
        .setTransport(transport)
        .setClientSecrets(clientId, secret).build()
        .setFromTokenResponse(tokenResponse)

      val oauth2: Oauth2 = new Oauth2.Builder(
        transport, jsonFactory, credential).build()

      val tokenInfo: Tokeninfo = oauth2.tokeninfo().setAccessToken(credential.getAccessToken).execute()


      if (tokenInfo.containsKey("error")) {
        play.Logger.error(s"Authorizationtoken has been denied by Google")
        Future(Right(AuthenticationError("Authorizationtoken has been denied by Google", Some(tokenInfo))))
      } else if (!tokenInfo.getIssuedTo.equals(clientId)) {
        play.Logger.error(s"client_id doesn't match expected client_id")
        Future(Right(AuthenticationError("client_id doesn't match expected client_id", Some(tokenInfo))))
      } else if (isOnWhiteList(tokenInfo.getEmail)) {
        Future(Left(tokenInfo))
      } else {
        Future(Right(AuthenticationError("Unable to request authorization to Google ", Some(tokenInfo))))
      }
    } catch {
      case tre: TokenResponseException => {
        play.Logger.error("Unable to request authorization to Google " + tre)
        Future(Right(AuthenticationError("Unable to request authorization to Google " + tre, None)))
      }
      case ioe: IOException => {
        play.Logger.error("Unable to request authorization to Google " + ioe)
        Future(Right(AuthenticationError("Unable to request authorization to Google " + ioe, None)))
      }
    }
  }

  def isOnWhiteList(email:String) = {
    val filteredUsers = Seq(email)
    val whitelist = Play.configuration.getString("whitelist").getOrElse("").split(",")
    (filteredUsers ++ whitelist).contains(email)
  }
}



case class AuthenticationError(message: String, tokenInfo: Option[Tokeninfo])

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
 def IsAuthenticated(f: => (String, String) => Request[AnyContent] => Future[Result]) =
   Security.Authenticated(username, onUnauthorized) { case (username, name) =>
     Action.async(request => f(username, name)(request))
 }

}
