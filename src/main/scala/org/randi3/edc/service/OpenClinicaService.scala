package org.randi3.edc.service

import org.randi3.model.{TrialSite, TrialSubject}

import scala.xml._
import org.randi3.edc.model.openClinica.ConnectionOC

import org.randi3.openclinica.cdisc.CDISCToModelConverter._


import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.randi3.edc.model.openClinica.TrialOC
import scalaz.Validation
import org.randi3.edc.dao.OpenClinicaDaoComponent
import org.randi3.dao.UserDaoComponent
import org.randi3.utility.SecurityComponent


trait OpenClinicaServiceComponent {
  this: OpenClinicaDaoComponent with
    UserDaoComponent with
    SecurityComponent =>


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

  private def message(bodyContent: Elem, beanSchema: String, username: String, passwordHash: String): Elem = {
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v1={ beanSchema } xmlns:bean="http://openclinica.org/ws/beans">
      { header(username, passwordHash ) }
      <soapenv:Body>
        { bodyContent }
      </soapenv:Body>
    </soapenv:Envelope>
  }

  def getTrials(connection: ConnectionOC): List[TrialOC] = {
    val request = studyMessage(<v1:listAllRequest/>, connection.username, connection.passwordHash)
    val result = sendMessage(connection.location + "/ws/study/v1", request)
    //TODO Error handling
    createTrialOCsWithoutEventsAndMetadata(connection, (result.get \\ "study"))
  }

  private def createTrialOCsWithoutEventsAndMetadata(connection: ConnectionOC, studySeqs: NodeSeq): List[TrialOC] = {
    if (studySeqs.isEmpty) return List()
    new TrialOC(identifier = (studySeqs.head \ "identifier").text, oid = (studySeqs.head \ "oid").text, name = (studySeqs.head \ "name").text, description = "",trial = None, connection = connection) :: createTrialOCsWithoutEventsAndMetadata(connection, studySeqs.tail)
  }

  def getFullTrialOC(trialOC: TrialOC): Option[TrialOC] = {
    val request = studyMessage(<v1:getMetadataRequest>
                                 <v1:studyMetadata>
                                   <bean:identifier>{ trialOC.identifier }</bean:identifier>
                                 </v1:studyMetadata>
                               </v1:getMetadataRequest>, trialOC.connection.username, trialOC.connection.passwordHash)
    val result = sendMessage(trialOC.connection.location + "/ws/study/v1", request)
    //TODO Error handling
    val odm = XML.loadString((result.get \\ "odm").text)

    getFullTrialFromODM(odm, trialOC.connection)
  }

  def getSubjectList(trialOC: TrialOC): List[TrialSubject] = {
    val request = studySubjectMessage(<v1:listAllByStudyRequest>
                                        <bean:studyRef>
                                          <bean:identifier>{ trialOC.oid }</bean:identifier>
                                        </bean:studyRef>
                                      </v1:listAllByStudyRequest>, trialOC.connection.username, trialOC.connection.passwordHash)

    //TODO fix the problem with the invalid xml response

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

  def getSubjectsData(trialOC: TrialOC, dataSetId: Int, subjects: List[String]): List[TrialSubject] = {
    //TODO subjects
    val request = studySubjectMessage(<v1:getStudySubjectRequest>
                                        <v1:studyRef>
                                          <bean:identifier>{ trialOC.oid }</bean:identifier>
                                        </v1:studyRef>
                                        <v1:dataSetIdentifier>{ dataSetId}</v1:dataSetIdentifier>
                                        <v1:label>Patient1</v1:label>
                                      </v1:getStudySubjectRequest>, trialOC.connection.username,  trialOC.connection.passwordHash)
    val result = sendMessage(trialOC.connection.location + "/ws/studySubject/v1", request)
    val odm = XML.loadString((result.get \\ "odm").text)

    getAllSubjectsFromODM(odm, trialOC)
  }

  private def sendMessage(host: String, req: Elem): Option[Elem] = {
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
      Some(XML.load(conn.getInputStream()))
    } catch {
      case e: Exception =>
        error("post: " + e.getMessage)
        error("post:" + scala.io.Source.fromInputStream(conn.getErrorStream).mkString)
        None
    }
  }

  def getLocalTrials(): List[TrialOC] = {
    Nil
  }

  def createNewLocalTrial(trialOC: TrialOC): Validation[String, TrialOC] = {
    //TODO security
    openClinicaDao.create(trialOC)
  }

  def getLocalTrial(id: Int): Validation[String, TrialOC] = {
    //TODO security
    openClinicaDao.get(id)
  }

}
}
