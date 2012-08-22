package org.randi3.schema

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ ExtendedTable => Table }
import org.scalaquery.ql.extended._
import org.scalaquery.ql.basic._

object OpenClinicaDatabaseShema {

  val TrialOCs = new Table[(Int, String, String, String, String)]("TrialOC") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def identifier = column[String]("Identifier", O NotNull)
    def oid = column[String]("Oid", O NotNull)
    def name = column[String]("Name", O NotNull)
    def metaDataVersionOid = column[String]("metaDataVersionOid", O NotNull)
    def * = id ~ identifier ~ oid ~ name ~ metaDataVersionOid
    def noId = identifier ~ oid ~ name ~ metaDataVersionOid
    def uniqueIdentifier = index("uniqueTrialOCIdentifier", identifier, unique = true)
    def uniqueOid = index("uniqueTrialOCOid", oid, unique = true)
    def uniqueName = index("uniqueTrialOCName", name, unique = true)
  }

  val EventOCs = new Table[(Int, String, Int)]("EventOC") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def trialOCId = column[Int]("TrialOCId")
    def * = id ~ oid ~ trialOCId
    def noId = oid ~ trialOCId
    def uniqueOid = index("uniqueEventOCOid", oid, unique = true)
    def trialOC = foreignKey("TrialOCFK_EventOC", trialOCId, TrialOCs)(_.id)
  }

  val FormOCs = new Table[(Int, String, Int)]("FormOC") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def eventOCId = column[Int]("EventOCId")
    def * = id ~ oid ~ eventOCId
    def noId = oid ~ eventOCId
    def uniqueOid = index("uniqueFormOCOid", oid, unique = true)
    def evenOC = foreignKey("EventOCFK_FormOC", eventOCId, EventOCs)(_.id)
  }

  val ItemGroupOCs = new Table[(Int, String, Int)]("ItemGroupOC") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def formOCId = column[Int]("ItemGroupOCId")
    def * = id ~ oid ~ formOCId
    def noId = oid ~ formOCId
    def uniqueOid = index("uniqueItemGroupOCOid", oid, unique = true)
    def formOC = foreignKey("FormOCFK_ItemGroupOC", formOCId, FormOCs)(_.id)
  }
  
  val ItemOCs = new Table[(Int, String, Int, Int)]("ItemGroupOC") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def itemGroupOCId = column[Int]("ItemGroupOCId")
    def criterionId = column[Int]("CriterionId")
    def * = id ~ oid ~ itemGroupOCId ~ criterionId
    def noId = oid ~ itemGroupOCId ~ criterionId
    def uniqueOid = index("uniqueItemGroupOCOid", oid, unique = true)
    def itemGroupOC = foreignKey("ItemGroupOCFK_ItemOC", itemGroupOCId, ItemGroupOCs)(_.id)
  }
  
  def createDatabaseTables(db: Database, driver: ExtendedProfile) = {
    import driver.Implicit._
    db withSession {
      (TrialOCs.ddl ++ EventOCs.ddl ++ FormOCs.ddl ++ ItemGroupOCs.ddl ++ ItemOCs.ddl).create
    }
  }

}
