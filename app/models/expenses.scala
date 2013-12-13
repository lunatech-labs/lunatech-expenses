package models

import org.jboss.netty.buffer._
import org.joda.time.DateTime
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import org.joda.time.format._
import reactivemongo.bson._


case class RecurringExpense (
  id: Option[BSONObjectID],
  description: String,
  amount: Double,
  frequence: String,
  author: String
) 


object RecurringExpense {

   implicit object RecurringExpenseBSONReader extends BSONDocumentReader[RecurringExpense] {
    def read(doc: BSONDocument) =
      RecurringExpense(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("description").get,
        doc.getAs[BSONDouble]("amount").get.value,
        doc.getAs[String]("frequence").get,
        doc.getAs[String]("author").get)
  }

  implicit object RecurringExpenseBSONWriter extends BSONDocumentWriter[RecurringExpense] {
     def write(recurringExpense: RecurringExpense): BSONDocument =
      BSONDocument(
        "_id" -> recurringExpense.id.getOrElse(BSONObjectID.generate),
        "description" -> recurringExpense.description,
        "amount" -> BSONDouble(recurringExpense.amount),
        "frequence" -> recurringExpense.frequence,
        "author" -> recurringExpense.author)
  }
}
 
case class Expense (
  id: Option[BSONObjectID],
  status: String,
  reference: Option[String],
  author: String,
  startDate: DateTime,
  endDate: DateTime,
  items: Seq[Item] = Seq(),
  comments: Seq[Comment] = Seq()) {

  def totalAmount = items.map(_.amount).sum

}

case class Comment(id: Option[String], author: String, email: String, date: DateTime, content: String)

case class Item(date: DateTime, name: String, amount: Double, note: Option[String])

object Expense {

  implicit object ItemBSONReader extends BSONDocumentReader[Item] {
    def read(doc: BSONDocument) =
      Item(
        doc.getAs[BSONDateTime]("date").map(dt => new DateTime(dt.value)).get,
        doc.getAs[String]("name").get,
        doc.getAs[BSONDouble]("amount").get.value,
        doc.getAs[String]("note")
      )
  }

  implicit object ItemBSONWriter extends BSONDocumentWriter[Item] {
     def write(item: Item): BSONDocument =
      BSONDocument(
        "date" -> BSONDateTime(item.date.getMillis),
        "name" -> item.name,
        "amount" -> BSONDouble(item.amount),
        "note" -> item.note)
  }

  implicit object CommentBSONReader extends BSONDocumentReader[Comment] {
    def read(doc: BSONDocument) =
      Comment(
        doc.getAs[String]("id"),
        doc.getAs[String]("author").get,
        doc.getAs[String]("email").get,
        doc.getAs[BSONDateTime]("date").map(dt => new DateTime(dt.value)).get,
        doc.getAs[String]("content").get
      )
  }

  implicit object CommentBSONWriter extends BSONDocumentWriter[Comment] {
     def write(comment: Comment): BSONDocument =
      BSONDocument(
        "id" -> comment.id.getOrElse(BSONObjectID.generate.stringify),
        "author" -> comment.author,
        "email" -> comment.email,
        "date" -> BSONDateTime(comment.date.getMillis),
        "content" -> comment.content)
  }

  implicit object ExpenseBSONReader extends BSONDocumentReader[Expense] {
    def read(doc: BSONDocument): Expense =
      Expense(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("status").get,
        doc.getAs[String]("reference"),
        doc.getAs[String]("author").get,
        doc.getAs[BSONDateTime]("start_date").map(dt => new DateTime(dt.value)).get,
        doc.getAs[BSONDateTime]("end_date").map(dt => new DateTime(dt.value)).get,
        doc.getAs[Seq[Item]]("items").get,
        doc.getAs[Seq[Comment]]("comments").get
      )
  }
  implicit object ExpenseBSONWriter extends BSONDocumentWriter[Expense] {
    def write(expense: Expense): BSONDocument =
      BSONDocument(
        "_id" -> expense.id.getOrElse(BSONObjectID.generate),
        "status" -> expense.status,
        "reference" -> expense.reference,
        "author" -> expense.author,
        "start_date" -> BSONDateTime(expense.startDate.getMillis),
        "end_date" -> BSONDateTime(expense.endDate.getMillis),
        "items" -> expense.items,
        "comments" -> expense.comments,
        "year" -> expense.startDate.getYear // TODO: get the year from the start date
      )
  }
}