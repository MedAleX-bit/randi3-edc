package org.randi3.edc.service
import java.util.Date
import org.junit.runner.RunWith
import org.randi3.model.{TrialStatus, StratifiedTrialSite, TrialSubjectIdentificationCreationType, Trial}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSpec

import org.scalatest.junit.JUnitRunner
import org.joda.time.LocalDate

@RunWith(classOf[JUnitRunner])
class OpenClinicaServiceSpec extends FunSpec with MustMatchers with ShouldMatchers {

  describe("The OpenClinicaService getTrials method") {

    it("text") {
      val service = new OpenClinicaService
    }

    it("test1") {
      val service = new OpenClinicaService
      println(service.getTrials())
    }

    it("test2") {
      val service = new OpenClinicaService
      val trialOC = service.getFullTrialOC("TestTrial").get
       println("-------------- "+trialOC)
      val trial = Trial(name = trialOC.identifier, abbreviation = "abb" , description = trialOC.identifier, startDate= new LocalDate(), endDate = new LocalDate(), stratifyTrialSite = StratifiedTrialSite.NO, status = TrialStatus.ACTIVE, treatmentArms = Nil, criterions = trialOC.getAllCriteria(), participatingSites = List(), randomizationMethod = None, stages = Map(), identificationCreationType = TrialSubjectIdentificationCreationType.EXTERNAL).toOption.get
//      println(trial)
      val list = service.getSubjectsData(trial, 2, List("dsSubject1"))
      println(list)
    }

    it("test") {
      val service = new OpenClinicaService
      val list = service.getSubjectList("TestTrial")
      println(list)
    }
  }

}
