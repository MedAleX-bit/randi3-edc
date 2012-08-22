package org.randi3.openclinica.service
import java.util.Date
import org.junit.runner.RunWith
import org.randi3.model.Trial
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner
import org.randi3.openclinica.model.TrialOC

@RunWith(classOf[JUnitSuiteRunner])
class OpenClinicaServiceSpec extends Spec with MustMatchers with ShouldMatchers {

  describe("The OpenClinicaService getTrials method") {

    it("text") {
      val service = new OpenClinicaService
    }

    it("test1") {
      val service = new OpenClinicaService
    }

    it("test2") {
      val service = new OpenClinicaService
      val trialOC = service.getFullTrialOC("default-study").get

      val trial = new Trial(name = trialOC.identifier, abbreviation = "abb" , description = trialOC.identifier, startDate= new Date, endDate = new Date, openTrial = true, treatmentArms = Nil, criterions = trialOC.getAllCriteria(), participatingSites = List(),
			    stageCount = 1, randomizationMethod = null)
//      println(trial)
      val list = service.getSubjectsData(trial, 2, List("dsSubject1"))
      println(list)
    }

    it("test") {
      val service = new OpenClinicaService
      val list = service.getSubjectList("defalult-study")
    }
  }

}
