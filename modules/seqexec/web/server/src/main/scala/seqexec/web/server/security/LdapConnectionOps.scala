// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import com.unboundid.ldap.sdk.{SearchRequest, SearchScope, _}
import seqexec.model.UserDetails._

import scala.collection.JavaConverters._

object LdapConnectionOps {
  // Extension methods for ldap connection
  implicit class LdapConnectionOps(val c: LDAPConnection) extends AnyVal {
    def authenticate(u: String, p: String): UID = {
      val UidExtractor = s"(\\w*)@(.*)?".r

      val bindRequest = new SimpleBindRequest(u, p)
      // Authenticate, it throws an exception if it fails
      c.bind(bindRequest)
      // Uid shouldn't have domain
      u match {
        case UidExtractor(uid, _) => uid
        case uid                  => uid
      }
    }

    def displayName(uid: UID): DisplayName = {
      val dn = for {
          a <- attributes(uid, List("displayName")).get("displayName")
          d <- a.headOption
        } yield d
      dn.getOrElse("-")
    }

    def nameGroupsThumb(uid: UID): (DisplayName, Groups, Option[Thumbnail]) = {
      val attrs = attributes(uid, List("displayName", "memberOf", "thumbnailPhoto"))

      val dn = for {
          a <- attrs.get("displayName")
          d <- a.headOption
        } yield d

      // Read the groups
      val gr = attrs.getOrElse("memberOf", Nil)
      val groups = gr.map { g =>
        val grDN = new DN(g)
        for {
          rdn <- grDN.getRDNs.toList
          if rdn.hasAttribute("CN") && !rdn.hasAttributeValue("CN", "Users")
        } yield rdn.getAttributeValues.toList
      }

      // Read the thumbnail if possible
      val thBytes = for {
          ph <- attrs.get("thumbnailPhoto")
          th <- ph.headOption
        } yield th.getBytes

      (dn.getOrElse("-"), groups.flatten.flatten, thBytes)
    }

    // Search for a user and find attributes. All attributes are String in LDAP
    private def attributes(uid: UID, attributes: List[String]): Map[String, List[String]] = {
      def readAttr(e: SearchResultEntry)(attr: String): Option[(String, List[String])] = {
        for {
          a <- Option(e.getAttribute(attr))
          d <- Option(a.getValues.toList)
        } yield attr -> d
      }

      val baseDN = c.getRootDSE.getAttributeValue("namingContexts")
      val filter = Filter.createANDFilter(
        Filter.createEqualityFilter("uid", uid),
        Filter.createEqualityFilter("objectClass", "user")
      )

      //val attributes = List("displayName", "memberOf", "thumbnailPhoto")
      val search = new SearchRequest(s"cn=users,$baseDN", SearchScope.SUB, filter, attributes: _*)
      // Search to read user data, it may throw an exception
      val searchResult = c.search(search)

      val r = for {
        s <- searchResult.getSearchEntries.asScala.headOption
      } yield attributes.map(readAttr(s))
      r.map(_.collect { case Some(i) => i }.toMap).getOrElse(Map.empty)
    }
  }
}
