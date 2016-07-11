package net.liftweb.mockweb

import net.liftweb.common.{Empty, Full}
import net.liftweb.http._
import net.liftweb.mockweb.snippet.WebSpecSpecSnippet
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import scala.xml.NodeSeq


/**
  * @author eiennohito
  * @since 2016/07/11
  */
class InstantiatedSnippetSpec extends Specification with XmlMatchers {
  val mockedRules = new LiftRules

  LiftRulesMocker.devTestLiftRulesInstance.doWith(mockedRules) {
    mockedRules.snippetInstantiation = Full(mockInstance())
    mockedRules.addToPackages("net.liftweb.mockweb")
  }

  def mockInstance() = new SnippetInstantiation {
    override def factoryFor[T](clz: Class[T]) = {
      if (clz.equals(classOf[WebSpecSpecSnippet])) {
        Full(SnippetInstantiation((_, _) => InstantiatedSnippetSpec.this))
      } else Empty
    }
  }

  def test: NodeSeq = <div id="asd" />

  "InstantiatedSnippet" should {
    "resolve to correct snippet" in {
      val hml = <div class="lift:WebSpecSpecSnippet.test"></div>

      LiftRulesMocker.devTestLiftRulesInstance.doWith(mockedRules) {
        S.statelessInit(Req.nil) {
          for (s <- S.session) {
            val res = s.processSurroundAndInclude("index", hml)
            res must ==/ (test)
          }
        }
      }

    }
  }
}
