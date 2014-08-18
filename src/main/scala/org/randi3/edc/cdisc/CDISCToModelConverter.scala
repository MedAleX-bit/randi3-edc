package org.randi3.openclinica.cdisc

import org.randi3.model.{ TrialSite, SubjectProperty, Trial, TrialSubject }
import scala.xml.NodeSeq
import org.randi3.model.criterion._
import constraint.Constraint
import scala.annotation.tailrec
import org.randi3.edc.model.openClinica._
import scala.Some
import org.randi3.edc.model.openClinica.ItemOC
import org.randi3.edc.model.openClinica.FormOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import org.randi3.edc.model.openClinica.EventOC

object CDISCToModelConverter {

  def getFullTrialFromODM(odm: NodeSeq, connection: ConnectionOC): Option[TrialOC] = {
    val odmStudy = (odm \ "Study").head
    val oid = ((odmStudy \\ "Study") \ "@OID").text
    val name = (odmStudy \\ "StudyName").text
    val identifier = (odmStudy \\ "ProtocolName").text
    val description = (odmStudy \\ "StudyDescription").text
    Some(new TrialOC(oid = oid, name = name, identifier = identifier, description = description, metaDataVersionOID = ((odmStudy \\ "MetaDataVersion")(0) \ "@OID").text, events = extractEvents((odmStudy \\ "StudyEventDef"), odmStudy), trial = None, connection = connection, treatmentItem = None))
  }

  private def extractEvents(eventSeq: NodeSeq, fullSeq: NodeSeq): List[EventOC] = {
    @tailrec def iter(eventSeq: NodeSeq, fullSeq: NodeSeq, acc: List[EventOC]): List[EventOC] = {
      if (eventSeq.isEmpty) return acc
      iter(eventSeq.tail, fullSeq, new EventOC(oid = (eventSeq.head \ "@OID").text, forms = extractForms((eventSeq.head \ "FormRef"), fullSeq)) :: acc)
    }
    iter(eventSeq, fullSeq, Nil)
  }

  // TODO TailRec
  private def extractForms(formSeq: NodeSeq, fullSeq: NodeSeq): List[FormOC] = {
    if (formSeq.isEmpty) return List()
    val oid = (formSeq.head \ "@FormOID").text
    val itemGroupSeq = ((fullSeq \\ "FormDef").filter(formDef => (formDef \ "@OID").text == oid) \\ "ItemGroupRef")
    new FormOC(oid, extractItemGroups(itemGroupSeq, fullSeq)) :: extractForms(formSeq.tail, fullSeq)
  }

  // TODO TailRec
  private def extractItemGroups(itemGroupSeq: NodeSeq, fullSeq: NodeSeq): List[ItemGroupOC] = {
    if (itemGroupSeq.isEmpty) return List()
    val oid = (itemGroupSeq.head \ "@ItemGroupOID").text
    val itemSeq = ((fullSeq \\ "ItemGroupDef").filter(itemGroupDef => (itemGroupDef \ "@OID").text == oid) \ "ItemRef")
    new ItemGroupOC(oid, extractItems(itemSeq, fullSeq)) :: extractItemGroups(itemGroupSeq.tail, fullSeq)
  }

  // TODO TailRec
  private def extractItems(itemSeq: NodeSeq, fullSeq: NodeSeq): List[ItemOC] = {
    if (itemSeq.isEmpty) return List()
    val head: NodeSeq = itemSeq.head
    val oid = (head \ "@ItemOID").text
    val singleItemSeq = ((fullSeq \\ "ItemDef").filter(itemDef => (itemDef \ "@OID").text == oid))
    val itemOC =
      if (!(singleItemSeq \\ "CodeListRef").isEmpty) {
        val ordinalCrit = extractOrdinalCriterion(singleItemSeq, fullSeq)
        new ItemOC(oid, ordinalCrit._1.asInstanceOf[Criterion[Any, Constraint[Any]]], ordinalCrit._2)
      } else if (!(singleItemSeq \\ "MultiSelectListRef").isEmpty) {
        //Do nothing, can't handle multi selectable elements
        null
      } else {
        new ItemOC(oid, extractSimpleCriterion(singleItemSeq).asInstanceOf[Criterion[Any, Constraint[Any]]])
      }
    val itemOCs = if (itemOC == null) Nil else List(itemOC)
    itemOCs ::: extractItems(itemSeq.tail, fullSeq)
  }

  private def extractOrdinalCriterion(itemSeq: NodeSeq, fullSeq: NodeSeq): (Criterion[_ <: Any, Constraint[_ <: Any]], Map[String, String]) = {
    val itemOid = ((itemSeq \ "ItemDetails") \ "@ItemOID").text
    val description = if (!(itemSeq \ "@Comment").text.isEmpty) (itemSeq \ "@Comment").text else "Description not set"
    val codeListOid = ((itemSeq \\ "CodeListRef") \ "@CodeListOID").text
    val codeListRef = (fullSeq \\ "CodeList").filter(codeList => (codeList \ "@OID").text == codeListOid)
    val extractedValues = extractCodeListItems(codeListRef)
    (OrdinalCriterion(name = itemOid, description = description, values = extractedValues.values.toSet, inclusionConstraint = None, strata = Nil).toOption.get, extractedValues)
  }

  private def extractCodeListItems(codeSeq: NodeSeq): Map[String, String] = {
    @tailrec def extractCodeValue(codeValueSeq: NodeSeq, acc: List[(String, String)]): Map[String, String] = {
      if (codeValueSeq.isEmpty) return acc.toMap
      val value = (codeValueSeq.head \ "@CodedValue").text
      val deCodedValue = (codeValueSeq.head \\ "TranslatedText").text
      extractCodeValue(codeValueSeq.tail, (value, deCodedValue) :: acc)
    }
    extractCodeValue((codeSeq \\ "CodeListItem"), Nil)
  }

  private def extractSimpleCriterion(itemSeq: NodeSeq): Criterion[_ <: Any, Constraint[_ <: Any]] = {
    val itemOid = ((itemSeq \ "ItemDetails") \ "@ItemOID").text
    val description = (itemSeq \ "@Comment").text
    (itemSeq \ "@DataType").text match {
      // text is not necessary for the randomization
      // case "text" => FreeTextCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case "integer" => IntegerCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case "float" => DoubleCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      //Partial date does not map to date criterion
      case "data" => DateCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case _ => null
    }
  }

  def getAllSubjectsFromODM(odm: NodeSeq, trialOC: TrialOC): List[TrialSubject] = {
    extractClinicalData(odm \\ "ClinicalData", trialOC)
  }

  private def extractClinicalData(odm: NodeSeq, trialOC: TrialOC): List[TrialSubject] = {
    odm.flatMap(clinicalData => {
      val studyOID = clinicalData.attribute("StudyOID") match {
        case None => "NoStudyOID"
        case Some(node) => node.text
      }
      val siteName = trialOC.sites.find(ocSite => ocSite._2 == studyOID).getOrElse(("NoSiteFound", "NoSiteFound"))._1
      val dummyTrialSite = TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "password", true).toOption.get
      if (trialOC.trial.isDefined)
        extractSubjects(clinicalData \\ "SubjectData", trialOC.trial.get.criterions, trialOC, trialOC.trial.get.participatingSites.find(site => site.name == siteName).getOrElse(trialOC.trial.get.participatingSites.head))
      else
        extractSubjects(clinicalData \\ "SubjectData", trialOC.getAllCriteria(), trialOC, dummyTrialSite)
    }).toList
  }

  private def extractSubjects(subjectNodes: NodeSeq, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], trialOC: TrialOC, site: TrialSite): List[TrialSubject] = {
    if (subjectNodes.isEmpty) return List()
    val subjectProperties = extractSubjectProperties(subjectNodes.head \\ "ItemData", criterions, trialOC)
    val identifier = subjectNodes.head.attribute(subjectNodes.head.getNamespace("OpenClinica"), "StudySubjectID").get.text + "->" + (subjectNodes.head \ "@SubjectKey").text
    //TODO openclinica field for userid
    val subject = TrialSubject(identifier = identifier, investigatorUserName = "dummyInvestigator", properties = subjectProperties, trialSite = site).toOption.get
    subject :: extractSubjects(subjectNodes.tail, criterions, trialOC, site)
  }

  private def extractSubjectProperties(itemNodes: NodeSeq, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]], trialOC: TrialOC): List[SubjectProperty[Any]] = {
    if (itemNodes.isEmpty) return List()
    val itemOID = (itemNodes.head \ "@ItemOID").text
    //if no criterion with the itemOid exists try next itemNode
    //TODO change value if ordinal criterion -> mapping in itemOC
    val criterion = criterions.find(crit => crit.name == itemOID).orElse(return extractSubjectProperties(itemNodes.tail, criterions, trialOC)).get.asInstanceOf[Criterion[Any, Constraint[Any]]]
    val value = if (criterion.isInstanceOf[OrdinalCriterion]) {
      trialOC.getMappedElementsFromCriteria(criterion).get._4.ordinalValueMapping.get((itemNodes.head \ "@Value").text).get
    } else {
      val text = (itemNodes.head \ "@Value").text
      if(criterion.isInstanceOf[IntegerCriterion]){
        text.toInt
      }else if(criterion.isInstanceOf[DoubleCriterion]) {
        text.toDouble
      }else if(criterion.isInstanceOf[DateCriterion]) {
        //TODO
        text
      }else text
    }
    SubjectProperty(criterion = criterion.asInstanceOf[Criterion[Any, Constraint[Any]]], value = value).toOption.get :: extractSubjectProperties(itemNodes.tail, criterions, trialOC)
  }

}
