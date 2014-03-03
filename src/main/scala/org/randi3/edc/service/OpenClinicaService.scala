package org.randi3.edc.service

import org.randi3.model._

import scala.xml._
import org.randi3.edc.model.openClinica.ConnectionOC

import org.randi3.openclinica.cdisc.CDISCToModelConverter._

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.randi3.edc.model.openClinica.TrialOC
import scalaz.{ Success, Failure, Validation }
import org.randi3.edc.dao.OpenClinicaDaoComponent
import org.randi3.dao.{ RandomizationMethodDaoComponent, TrialSiteDaoComponent, TrialRightDaoComponent, TrialSubjectDaoComponent, TrialDaoComponent, UserDaoComponent }
import org.randi3.utility.{ UtilityMailComponent, MailSenderComponent, SecurityComponent }
import scala.collection.mutable.ListBuffer
import scalaz.Failure
import scala.Some
import org.randi3.edc.model.openClinica.ConnectionOC
import scalaz.Success
import org.randi3.edc.model.openClinica.TrialOC
import org.joda.time.format.DateTimeFormat
import scala.collection._

trait OpenClinicaServiceComponent {
  this: OpenClinicaDaoComponent with UserDaoComponent with TrialSiteDaoComponent with TrialDaoComponent with TrialSubjectDaoComponent with TrialRightDaoComponent with RandomizationMethodDaoComponent with MailSenderComponent with UtilityMailComponent with SecurityComponent =>

  val openClinicaService: OpenClinicaService

  class OpenClinicaService {

    private def header(user: String, passwordHash: String): Elem = {
      <soapenv:Header>
        <wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">
          <wsse:UsernameToken wsu:Id="UsernameToken-27777511" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">
            <wsse:Username>{ user }</wsse:Username>
            <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">{ passwordHash }</wsse:Password>
          </wsse:UsernameToken>
        </wsse:Security>
      </soapenv:Header>
    }

    private def studyMessage(bodyContent: Elem, username: String, passwordHash: String): Elem = {
      message(bodyContent, "http://openclinica.org/ws/study/v1", username, passwordHash)
    }

    private def studySubjectMessage(bodyContent: Elem, username: String, passwordHash: String): Elem = {
      message(bodyContent, "http://openclinica.org/ws/studySubject/v1", username, passwordHash)
    }

    private def studyEventMessage(bodyContent: Elem, username: String, passwordHash: String): Elem = {
      message(bodyContent, "http://openclinica.org/ws/event/v1", username, passwordHash)
    }

    private def dataImportMessage(bodyContent: Elem, username: String, passwordHash: String): Elem = {
      message(bodyContent, "http://openclinica.org/ws/data/v1", username, passwordHash)
    }

    private def message(bodyContent: Elem, beanSchema: String, username: String, passwordHash: String): Elem = {
      <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v1={ beanSchema } xmlns:bean="http://openclinica.org/ws/beans">
        { header(username, passwordHash) }
        <soapenv:Body>
          { bodyContent }
        </soapenv:Body>
      </soapenv:Envelope>
    }

    def getTrials(connection: ConnectionOC): List[TrialOC] = {
      val request = studyMessage(<v1:listAllRequest/>, connection.username, connection.passwordHash)
      val result = sendMessage(connection.location + "/ws/study/v1", request)
      //TODO Error handling
      val localTrials = openClinicaDao.getAllLocalTrialOids.toOption.getOrElse(List()) 
      createTrialOCsWithoutEventsAndMetadata(connection, (result.toOption.getOrElse(List()) \\ "study")).filter(trial => !localTrials.contains(trial.oid))
    }

    private def createTrialOCsWithoutEventsAndMetadata(connection: ConnectionOC, studySeqs: NodeSeq): List[TrialOC] = {
      if (studySeqs.isEmpty) return List()

      def extractSites(element: NodeSeq): immutable.Map[String, String] = {
        val tmpMap = new mutable.HashMap[String, String]()
        for (site <- (element \ "site")) tmpMap.put((site \ "name").text, (site \ "oid").text)
        tmpMap.toMap
      }

      new TrialOC(identifier = (studySeqs.head \ "identifier").text, oid = (studySeqs.head \ "oid").text, name = (studySeqs.head \ "name").text, description = "", trial = None, connection = connection, treatmentItem = None, sites = extractSites(studySeqs.head \ "sites")) :: createTrialOCsWithoutEventsAndMetadata(connection, studySeqs.tail)
    }

    def getFullTrialOC(trialOC: TrialOC): Option[TrialOC] = {
      val request = studyMessage(<v1:getMetadataRequest>
                                   <v1:studyMetadata>
                                     <bean:identifier>{ trialOC.identifier }</bean:identifier>
                                   </v1:studyMetadata>
                                 </v1:getMetadataRequest>, trialOC.connection.username, trialOC.connection.passwordHash)
      val result = sendMessage(trialOC.connection.location + "/ws/study/v1", request)
      //TODO Error handling
      val odm = XML.loadString((result.toOption.getOrElse(return None) \\ "odm").text)
      getFullTrialFromODM(odm, trialOC.connection) match {
        case Some(fullTrial) => Some(fullTrial.copy(sites = trialOC.sites))
        case None => None
      }
    }

    def getSubjectList(trialOC: TrialOC): List[TrialSubject] = {
      val request = studySubjectMessage(<v1:listAllByStudyRequest>
                                          <bean:studyRef>
                                            <bean:identifier>{ trialOC.identifier }</bean:identifier>
                                          </bean:studyRef>
                                        </v1:listAllByStudyRequest>, trialOC.connection.username, trialOC.connection.passwordHash)

      val url = new java.net.URL(trialOC.connection.location + "/ws/studySubject/v1")
      val outs = request.toString.getBytes
      val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
      conn.setRequestMethod("POST")
      conn.setDoOutput(true)
      conn.setRequestProperty("Content-Length", outs.length.toString)
      conn.setRequestProperty("Content-Type", "text/xml")
      conn.getOutputStream.write(outs)
      conn.getOutputStream.close
      val response = new StringBuffer
      scala.io.Source.fromInputStream(conn.getInputStream(), "UTF-8").getLines().foreach { line =>
        if (line.contains("SOAP-ENV:Envelope")) response.append(line)
      }

      val is = new java.io.ByteArrayInputStream(response.toString().getBytes("UTF-8"))
      val isource = new org.xml.sax.InputSource(is)

      val parserFactory = new SAXFactoryImpl
      val parser = parserFactory.newSAXParser()
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      val result = Some(adapter.loadXML(isource, parser))
      extractSubjectsOnlyWithIdentifier(result.get \\ "studySubject")
    }

    private def extractSubjectsOnlyWithIdentifier(subjectNodes: NodeSeq): List[TrialSubject] = {
      if (subjectNodes.isEmpty) return List()
      val dummySite = TrialSite(Int.MinValue, 0, "validName", "validCountry", "validStreet", "validPostCode", "validCity", "password", true).toOption.get
      TrialSubject(identifier = (subjectNodes.head \\ "label").text, properties = Nil, investigatorUserName = "dummy user", trialSite = dummySite).toOption.get :: extractSubjectsOnlyWithIdentifier(subjectNodes.tail)
    }

    private def getSubjectsData(trialOC: TrialOC, dataSetId: Int, subjects: List[String]): List[TrialSubject] = {
      val request = dataImportMessage(<v1:exportDataSetRequest>
                                        <v1:studyRef>
                                          <bean:identifier>{ trialOC.identifier }</bean:identifier>
                                        </v1:studyRef>
                                        <v1:dataSetIdentifier>{ dataSetId }</v1:dataSetIdentifier>
                                      </v1:exportDataSetRequest>, trialOC.connection.username, trialOC.connection.passwordHash)
      val result = sendMessage(trialOC.connection.location + "/ws/data/v1", request)
      val odm = XML.loadString((result.toOption.getOrElse(return List()) \\ "odm").text)
      getAllSubjectsFromODM(odm, trialOC)
    }

    private def sendMessage(host: String, req: Elem): Validation[String, Elem] = {
      val url = new java.net.URL(host)
      val outs = req.toString.getBytes
      val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
      try {
        conn.setRequestMethod("POST")
        conn.setDoOutput(true)
        conn.setRequestProperty("Content-Length", outs.length.toString)
        conn.setRequestProperty("Content-Type", "text/xml")
        conn.getOutputStream.write(outs)
        conn.getOutputStream.close
        Success(XML.load(conn.getInputStream()))
      } catch {
        case e: Exception =>
 //         error("post: " + e.getMessage)
 //         error("post:" + scala.io.Source.fromInputStream(conn.getErrorStream).mkString)
          Failure(e.getMessage)
      }
    }

    def getLocalTrials(): Validation[String, List[TrialOC]] = {
      val user = securityUtility.currentUser.getOrElse(return Failure("Not logged in"))
      openClinicaDao.getAll().toEither match {
        case Left(error) => Failure(error)
        case Right(list) => {
          Success(list.filter(trialOC =>
            user.rights.map(right => right.trial.id).contains(trialOC.trial.get.id)))
        }
      }
    }

    def createNewLocalTrial(trialOC: TrialOC): Validation[String, TrialOC] = {
      val user = securityUtility.currentUser.getOrElse(return Failure("Not logged in"))
      if (!user.canCreateTrial) return Failure("User can't create a new trial")

      trialSiteDao.getAll.toEither match {
        case Left(failure) => Failure(failure)
        case Right(availableSites) => {
          val newSites = trialOC.sites.filter(entry => !availableSites.map(_.name).contains(entry._1))
          newSites.foreach(siteName => {
            val site: TrialSite = TrialSite(name = siteName._1, country = "Country", street = "Street", postCode = "PostCode", city = "City", password = "password", isActive = true).toOption.get
            trialSiteDao.create(site)
          })
          trialSiteDao.getAll.toEither match {
            case Left(failure) => Failure(failure)
            case Right(allSites) => {
              val trialOCwithTrialSites = trialOC.trial match {
                case None => trialOC
                case Some(trial) => {
                  trialOC.copy(trial = Some(trial.copy(participatingSites = (user.site :: allSites.filter(siteTMP => trialOC.sites.keys.toList.contains(siteTMP.name))).toSet.toList)))
                }
              }
              openClinicaDao.create(trialOCwithTrialSites).toEither match {
                case Left(error) => Failure(error)
                case Right(trialOC) => {
                  val trialRight = TrialRight(Role.trialAdministrator, trialOC.trial.get).toOption.get
                  trialRightDao.addRight(user.id, trialRight).toEither match {
                    case Left(error) => Failure("Trial added, but problem with the rights occurs (" + error + ")")
                    case Right(_) => Success(trialOC)
                  }
                }
              }
            }
          }
        }
      }
    }

    def getLocalTrial(id: Int, viaWebService: Boolean = false): Validation[String, TrialOC] = {
      val user = if(!viaWebService) securityUtility.currentUser.getOrElse(return Failure("Not logged in")) else null
      openClinicaDao.get(id).toEither match {
        case Left(error) => Failure(error)
        case Right(trialOC) => 
          if(viaWebService) Success(trialOC)
          else if (user.rights.map(right => right.trial.id).contains(trialOC.trial.get.id))
          Success(trialOC)
        else
          Failure("No Rights for the trial")
      }

    }

    def getLocalTrial(trialIdentifier: String, viaWebService: Boolean): Validation[String, TrialOC] = {
      openClinicaDao.getId(trialIdentifier).toEither match {
        case Left(failure) => Failure(failure)
        case Right(id) => getLocalTrial(id, viaWebService)
      }
    }

    def randomizeNewTrialSubjects(trial: TrialOC, viaWebService: Boolean = false): Validation[String, TrialOC] = {
      //TODO Security only invest pi ta
      val trialOC = getLocalTrial(trial.id, viaWebService).toOption.getOrElse(return Failure("OpenClinica trial not found"))
      val user = if (!viaWebService)
        securityUtility.currentUser.getOrElse(return Failure("Not logged in"))
      else {
        val dummySite = trialOC.trial.get.participatingSites.head
        User(username = "WebServiceAction", password = "password", email = "abc", firstName = "dummyUser", lastName = "dummyUser", phoneNumber = "123456", site = dummySite, rights = immutable.Set()).toOption.get
      }
      val trialSubjectIdentifier = trialOC.trial.getOrElse(return Failure("Trial not available")).getSubjects.map(subject => subject.identifier)

      //TODO synchronization of previous subjects
      
      val fullSubjects = getSubjectsData(trialOC, trialOC.connection.dataSetId, List()).filter(subject => !trialSubjectIdentifier.contains(subject.identifier))

      val dbTrial = trialOC.trial.get

      fullSubjects.foreach(trialSubject => {
        val subject = trialSubject.copy(investigatorUserName = user.username, trialSite = if (trialSubject.trialSite.id < 0) user.site else trialSubject.trialSite)
        dbTrial.randomize(subject).toEither match {
          case Left(x) =>
          case Right(treatmentArm) => {
            val subjectWithIdentification = subject.copy(identifier = TrialSubjectIdentificationCreator.createIdentification(dbTrial, treatmentArm, subject))
            trialSubjectDao.create(subjectWithIdentification, treatmentArm.id).toEither match {
              case Left(x) =>
              case _ => randomizationMethodDao.update(dbTrial.randomizationMethod.get).toEither match {
                case Left(x) =>
                case _ => {
                  mailSender.sendMessage(if(!viaWebService) user.email else "", utilityMail.getRandomizedMailCCAddresses(dbTrial), "", "[" + dbTrial.abbreviation + "] " + "Patient randomized", utilityMail.getRandomizedMailContent(dbTrial, treatmentArm, subjectWithIdentification))
                  writeRandomizationResult(trialOC, subjectWithIdentification, treatmentArm)
                  Success((treatmentArm, subjectWithIdentification.identifier))
                }
              }
            }
          }
        }
      })
      getLocalTrial(trialOC.id, viaWebService)
    }

    def randomizeNewTrialSubjects(trialIdentifier: String): Validation[String, TrialOC] = {
      getLocalTrial(trialIdentifier, true).toEither match {
        case Left(failure) => Failure(failure)
        case Right(trialOC) => randomizeNewTrialSubjects(trialOC, true)
      }

    }

    private def writeRandomizationResult(trialOC: TrialOC, trialSubject: TrialSubject, treatmentArm: TreatmentArm): Validation[String, Boolean] = {
      scheduleEvent(trialOC, trialSubject, treatmentArm)
      importRandomizationResult(trialOC, trialSubject, treatmentArm)
      Success(true)
    }

    private def scheduleEvent(trialOC: TrialOC, trialSubject: TrialSubject, treatmentArm: TreatmentArm): Validation[String, Boolean] = {
      val request = studyEventMessage(<v1:scheduleRequest>
                                        <!--1 or more repetitions:-->
                                        <v1:event>
                                          <bean:studySubjectRef>
                                            <bean:label>{ trialSubject.identifier.split("->").head }</bean:label>
                                          </bean:studySubjectRef>
                                          <bean:studyRef>
                                            <bean:identifier>{ trialOC.identifier }</bean:identifier>
                                          </bean:studyRef>
                                          <bean:eventDefinitionOID>{ trialOC.treatmentItem.get._1.oid }</bean:eventDefinitionOID>
                                          <bean:location>RANDI2</bean:location>
                                          <bean:startDate>{ trialSubject.createdAt.toString(DateTimeFormat.forPattern("yyyy-MM-dd")) }</bean:startDate>
                                        </v1:event>
                                      </v1:scheduleRequest>,
        trialOC.connection.username, trialOC.connection.passwordHash)
      val result = sendMessage(trialOC.connection.location + "/ws/event/v1", request)
      //TODO http status codes
      Success(true)

    }

    private def importRandomizationResult(trialOC: TrialOC, trialSubject: TrialSubject, treatmentArm: TreatmentArm): Validation[String, Boolean] = {
      val request = dataImportMessage(<v1:importRequest>
                                        <ODM>
                                          <ClinicalData StudyOID={ trialOC.oid } MetaDataVersionOID={ trialOC.metaDataVersionOID }>
                                            <SubjectData SubjectKey={ trialSubject.identifier.split("->").last }>
                                              <StudyEventData StudyEventOID={ trialOC.treatmentItem.get._1.oid }>
                                                <FormData FormOID={ trialOC.treatmentItem.get._1.forms.head.oid }>
                                                  <ItemGroupData ItemGroupOID={ trialOC.treatmentItem.get._1.forms.head.items.head.oid } TransactionType="Insert">
                                                    <ItemData ItemOID={ trialOC.treatmentItem.get._1.forms.head.items.head.items.head.oid } Value={
                                                      trialOC.treatmentItem.get._1.forms.head.items.head.items.head.ordinalValueMapping.find(entry => entry._2 == treatmentArm.name).get._1
                                                    }/>
                                                  </ItemGroupData>
                                                </FormData>
                                              </StudyEventData>
                                            </SubjectData>
                                          </ClinicalData>
                                        </ODM>
                                      </v1:importRequest>,
        trialOC.connection.username, trialOC.connection.passwordHash)
      val result = sendMessage(trialOC.connection.location + "/ws/data/v1", request)
      //TODO http status codes
      Success(true)

    }

  }
}
