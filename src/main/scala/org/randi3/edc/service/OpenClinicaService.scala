package org.randi3.edc.service
import org.randi3.model.{TrialSite, TrialSubject, Trial}
import org.randi3.model.criterion._
import org.apache.commons.httpclient.HttpClient
import scala.xml._
import org.randi3.edc.model.openClinica._
import scala.annotation.tailrec
import org.randi3.openclinica.cdisc.CDISCUtility._

import org.xml.sax._
import parsing._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.randi3.edc.model.openClinica.TrialOC

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

  private def studyMessage(bodyContent: Elem): Elem = {
    message(bodyContent, "http://openclinica.org/ws/study/v1")
  }

  private def studySubjectMessage(bodyContent: Elem): Elem = {
    message(bodyContent, "http://openclinica.org/ws/studySubject/v1")
  }

  private def message(bodyContent: Elem, beanSchema: String): Elem = {
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v1={ beanSchema } xmlns:bean="http://openclinica.org/ws/beans">
      { header("root", "5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8") }
      <soapenv:Body>
        { bodyContent }
      </soapenv:Body>
    </soapenv:Envelope>
  }

  def getTrials(): List[TrialOC] = {
    val request = studyMessage(<v1:listAllRequest/>)
    val result = sendMessage("http://localhost:8080/OpenClinica-ws/ws/study/v1", request)
    //TODO Error handling
    createTrialOCsWithoutEventsAndMetadata((result.get \\ "study"))
  }

  private def createTrialOCsWithoutEventsAndMetadata(studySeqs: NodeSeq): List[TrialOC] = {
    if (studySeqs.isEmpty) return List()
    new TrialOC(identifier = (studySeqs.head \ "identifier").text, oid = (studySeqs.head \ "oid").text, name = (studySeqs.head \ "name").text) :: createTrialOCsWithoutEventsAndMetadata(studySeqs.tail)
  }

  def getFullTrialOC(trialIdentifier: String): Option[TrialOC] = {
    val request = studyMessage(<v1:getMetadataRequest>
                                 <v1:studyMetadata>
                                   <bean:identifier>{ trialIdentifier }</bean:identifier>
                                 </v1:studyMetadata>
                               </v1:getMetadataRequest>)
    val result = sendMessage("http://localhost:8080/OpenClinica-ws/ws/study/v1", request)
    //TODO Error handling
    val odm = XML.loadString((result.get \\ "odm").text)

    getFullTrialFromODM(odm)
  }

  def getSubjectList(trialIdentifier: String): List[TrialSubject] = {
    val request = studySubjectMessage(<v1:listAllByStudyRequest>
                                        <bean:studyRef>
                                          <bean:identifier>{ trialIdentifier }</bean:identifier>
                                        </bean:studyRef>
                                      </v1:listAllByStudyRequest>)

    //TODO fix the problem with the invalid xml response

    val url = new java.net.URL("http://localhost:8080/OpenClinica-ws/ws/studySubject/v1")
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

  def getSubjectsData(trial: Trial, dataSetId: Int, subjects: List[String]): List[TrialSubject] = {
    //TODO subjects
    val request = studySubjectMessage(<v1:getStudySubjectRequest>
                                        <v1:studyRef>
                                          <bean:identifier>{ trial.name }</bean:identifier>
                                        </v1:studyRef>
                                        <v1:dataSetIdentifier>{ dataSetId}</v1:dataSetIdentifier>
                                        <v1:label>Patient1</v1:label>
                                      </v1:getStudySubjectRequest>)
    val result = sendMessage("http://localhost:8080/OpenClinica-ws/ws/studySubject/v1", request)
    val odm = XML.loadString((result.get \\ "odm").text)

    getAllSubjectsFromODM(odm, trial)
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
        error("post: " + e)
        error("post:" + scala.io.Source.fromInputStream(conn.getErrorStream).mkString)
        None
    }
  }

}
