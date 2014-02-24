package org.randi3.dao

import org.junit.runner.RunWith
import org.randi3.schema.DatabaseSchema._
import org.randi3.edc.schema.OpenClinicaDatabaseSchema
import org.randi3.edc.utility.TestingEnvironmentEDC

import scala.slick.session.Database.threadLocalSession
import org.scalatest.matchers.MustMatchers
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.model.TrialSite
import scala.slick.lifted.Query

@RunWith(classOf[JUnitRunner])
class OpenClinicaDaoSpec extends FunSpec with ShouldMatchers{

  import TestingEnvironmentEDC._
  import schema._
  import openClinicaSchema._
  import TestingEnvironmentEDC.driver.Implicit._
 
  describe("The OpenClinicaDao create method") {

    it("should be able to create OCTrial with all fields and return the stored trial with all id's") {
      val trialOC = getTrialOC
      
      val res = openClinicaDao.create(trialOC) 
         
      res.toOption should not be(None)
      
      val trialOCDB = res.toOption.get
      
      trialOCDB.id should be > (0)
      trialOCDB.connection should not be(null)
      trialOCDB.connection.location should be(trialOC.connection.location)
      trialOCDB.connection.dataSetId should be(trialOC.connection.dataSetId)
      trialOCDB.connection.passwordHash should be(trialOC.connection.passwordHash)
      trialOCDB.connection.username should be(trialOC.connection.username)
      
      //TODO
    }

    
  }

}