package org.randi3.edc.schema


import org.randi3.schema.DatabaseSchema._

import org.randi3.schema.DatabaseSchema
import scala.slick.driver.ExtendedProfile

class OpenClinicaDatabaseSchema (val driver: ExtendedProfile) {
  import driver.Implicit._
  import driver.simple._

   val schema = new DatabaseSchema(driver)

  object TrialOCs extends Table[(Int, String, String, String, String, Int)]("OCTrial") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def identifier = column[String]("Identifier", O NotNull)
    def oid = column[String]("Oid", O NotNull)
    def name = column[String]("Name", O NotNull)
    def metaDataVersionOid = column[String]("metaDataVersionOid", O NotNull)
    def trialId = column[Int]("TrialId")
    def * = id ~ identifier ~ oid ~ name ~ metaDataVersionOid ~ trialId
    def noId = identifier ~ oid ~ name ~ metaDataVersionOid ~ trialId
    def uniqueIdentifier = index("uniqueTrialOCIdentifier", identifier, unique = true)
    def uniqueOid = index("uniqueTrialOCOid", oid, unique = true)
    def uniqueName = index("uniqueTrialOCName", name, unique = true)
    def trial = foreignKey("TrialOC_Trial", trialId, schema.Trials)(_.id)

  }

  object EventOCs extends Table[(Int, String, Int)]("OCEvent") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def trialOCId = column[Int]("TrialOCId")
    def * = id ~ oid ~ trialOCId
    def noId = oid ~ trialOCId
    def uniqueOid = index("uniqueEventOCOid", oid, unique = true)
    def trialOC = foreignKey("TrialOCFK_EventOC", trialOCId, TrialOCs)(_.id)
  }

  object FormOCs extends Table[(Int, String, Int)]("OCForm") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def eventOCId = column[Int]("EventOCId")
    def * = id ~ oid ~ eventOCId
    def noId = oid ~ eventOCId
    def uniqueOid = index("uniqueFormOCOid", oid, unique = true)
    def evenOC = foreignKey("EventOCFK_FormOC", eventOCId, EventOCs)(_.id)
  }

  object ItemGroupOCs extends Table[(Int, String, Int)]("OCItemGroup") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def formOCId = column[Int]("FormOCId")
    def * = id ~ oid ~ formOCId
    def noId = oid ~ formOCId
    def uniqueOid = index("uniqueItemGroupOCOid", oid, unique = true)
    def formOC = foreignKey("FormOCFK_ItemGroupOC", formOCId, FormOCs)(_.id)
  }
  
  object ItemOCs extends Table[(Int, String, Int, Int)]("OCItem") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def oid = column[String]("Oid", O NotNull)
    def itemGroupOCId = column[Int]("ItemGroupOCId")
    def criterionId = column[Int]("CriterionId")
    def * = id ~ oid ~ itemGroupOCId ~ criterionId
    def noId = oid ~ itemGroupOCId ~ criterionId
    def uniqueOid = index("uniqueItemGroupOCOid", oid, unique = true)
    def itemGroupOC = foreignKey("ItemGroupOCFK_ItemOC", itemGroupOCId, ItemGroupOCs)(_.id)
  }

  object Connections extends Table[(Int, Int, String, String, String)]("OCConnection") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)
    def trialOCId = column[Int]("TrialOCId")
    def location = column[String]("Location", O NotNull)
    def username = column[String]("Username", O NotNull)
    def passwordHash = column[String]("PasswordHash", O NotNull)
    def * = id ~ trialOCId ~ location ~ username ~ passwordHash
    def noId = trialOCId ~ location ~ username ~ passwordHash
    def trialOC = foreignKey("TrialOCFK_Connection", trialOCId, TrialOCs)(_.id)

  }

}
