// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.icon

import edu.gemini.seqexec.web.client.semanticui.Size
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import scalacss.ScalaCssReact._

/**
  * Semantic UI Icon component
  */
case class Icon(p: Icon.Props, children: Seq[VdomNode]) {
  import Icon._

  // Custom copy constructor to avoid passing the id again
  def copyIcon(disabled: Boolean = false,
           loading: Boolean = false,
           fitted: Boolean = false,
           size: Size = Size.NotSized,
           link: Boolean = false,
           flipped: Flipped = Flipped.NotFlipped,
           rotated: Rotated = Rotated.NotRotated,
           circular: Boolean = false,
           bordered: Boolean = false,
           inverted: Boolean = false,
           color: Option[String] = None,
           extraStyles: List[scalacss.internal.StyleA] = Nil,
           key: String = "",
           onClick: Callback = Callback.empty): Icon =
    copy(
      p = Icon.Props(id = p.id,
        disabled = disabled,
        loading = loading,
        fitted = fitted,
        size = size,
        link = link,
        flipped = flipped,
        rotated = rotated,
        circular = circular,
        bordered = bordered,
        inverted = inverted,
        color = color,
        extraStyles = extraStyles,
        key = key,
        onClick = onClick),
      children = if (children.nonEmpty) children else this.children)

  private def component = ScalaComponent.builder[Props]("Icon")
    .stateless
    .renderPC((_, p, c) =>
      <.i(
        ^.cls := s"${p.id} icon",
        p.extraStyles.map(scalacssStyleaToTagMod).toTagMod,
        ^.cls :=? p.color,
        ^.classSet(
          "disabled"                 -> p.disabled,
          "loading"                  -> p.loading,
          "fitted"                   -> p.fitted,
          "tiny"                     -> (p.size == Size.Tiny),
          "mini"                     -> (p.size == Size.Mini),
          "small"                    -> (p.size == Size.Small),
          "large"                    -> (p.size == Size.Large),
          "big"                      -> (p.size == Size.Big),
          "huge"                     -> (p.size == Size.Huge),
          "massive"                  -> (p.size == Size.Massive),
          "link"                     -> p.link,
          "horizontally flipped"     -> (p.flipped == Flipped.Horizontally),
          "vertically flipped"       -> (p.flipped == Flipped.Vertically),
          "clockwise rotated"        -> (p.rotated == Rotated.Clockwise),
          "counterclockwise rotated" -> (p.rotated == Rotated.CounterClockwise),
          "circular"                 -> p.circular,
          "bordered"                 -> p.bordered,
          "inverted"                 -> p.inverted
        ),
        ^.onClick --> p.onClick,
        c
      )
    )
    .build.withKey(p.key).apply(p)(children: _*)
}

object Icon {
  // Web content icons
  val IconAlarm             = Icon("alarm")
  val IconAlarmSlash        = Icon("alarm slash")
  val IconAlarmOutline      = Icon("alarm outline")
  val IconAlarmSlashOutline = Icon("alarm slash outline")
  val IconAt                = Icon("at")
  val IconBrowser           = Icon("browser")
  val IconBug               = Icon("bug")
  val IconCalendarOutline   = Icon("calendar outline")
  val IconCalendar          = Icon("calendar")
  val IconCloud             = Icon("cloud")
  val IconCode              = Icon("code")
  val IconComment           = Icon("comment")
  val IconComments          = Icon("comments")
  val IconCommentOutline    = Icon("comment outline")
  val IconCommentsOutline   = Icon("comments outline")
  val IconCopyright         = Icon("copyright")
  val IconDashboard         = Icon("dashboard")
  val IconDropdown          = Icon("dropdown")
  val IconExternalSquare    = Icon("external square")
  val IconExternal          = Icon("external")
  val IconEyedropper        = Icon("eyedropper")
  val IconFeed              = Icon("feed")
  val IconFind              = Icon("find")
  val IconHeartbeat         = Icon("heartbeat")
  val IconHistory           = Icon("history")
  val IconHome              = Icon("home")
  val IconIdea              = Icon("idea")
  val IconInbox             = Icon("inbox")
  val IconLab               = Icon("lab")
  val IconMail              = Icon("mail")
  val IconMailOutline       = Icon("mail outline")
  val IconMailSquare        = Icon("mail square")
  val IconMap               = Icon("map")
  val IconOptions           = Icon("options")
  val IconPaintBrush        = Icon("paint brush")
  val IconPayment           = Icon("payment")
  val IconPhone             = Icon("phone")
  val IconPhoneSquare       = Icon("phone square")
  val IconPrivacy           = Icon("privacy")
  val IconProtect           = Icon("protect")
  val IconSearch            = Icon("search")
  val IconSetting           = Icon("setting")
  val IconSettings          = Icon("settings")
  val IconShop              = Icon("shop")
  val IconSidebar           = Icon("sidebar")
  val IconSignal            = Icon("signal")
  val IconSitemap           = Icon("sitemap")
  val IconTag               = Icon("tag")
  val IconTags              = Icon("tags")
  val IconTasks             = Icon("tasks")
  val IconTerminal          = Icon("terminal")
  val IconTextTelephone     = Icon("text telephone")
  val IconTicket            = Icon("ticket")
  val IconTrophy            = Icon("trophy")
  val IconWifi              = Icon("wifi")

  // User action icons
  val IconAdjust                = Icon("adjust")
  val IconAddUser               = Icon("add user")
  val IconAddToCart             = Icon("add to cart")
  val IconArchive               = Icon("archive")
  val IconBan                   = Icon("ban")
  val IconBookmark              = Icon("bookmark")
  val IconCall                  = Icon("call")
  val IconCallSquare            = Icon("call square")
  val IconCloudDownload         = Icon("cloud download")
  val IconCloudUpload           = Icon("cloud upload")
  val IconCompress              = Icon("compress")
  val IconConfigure             = Icon("configure")
  val IconDownload              = Icon("download")
  val IconEdit                  = Icon("edit")
  val IconErase                 = Icon("erase")
  val IconExchange              = Icon("exchange")
  val IconExternalShare         = Icon("external share")
  val IconExpand                = Icon("expand")
  val IconFilter                = Icon("filter")
  val IconFlag                  = Icon("flag")
  val IconFlagOutline           = Icon("flag outline")
  val IconForwardMail           = Icon("forward mail")
  val IconHide                  = Icon("hide")
  val IconInCart                = Icon("in cart")
  val IconLock                  = Icon("lock")
  val IconPin                   = Icon("pin")
  val IconPrint                 = Icon("print")
  val IconRandom                = Icon("random")
  val IconRecycle               = Icon("recycle")
  val IconRefresh               = Icon("refresh")
  val IconRemoveBookmark        = Icon("remove bookmark")
  val IconRemoveUser            = Icon("remove user")
  val IconRepeat                = Icon("repeat")
  val IconReplyAll              = Icon("reply all")
  val IconReply                 = Icon("reply")
  val IconRetweet               = Icon("retweet")
  val IconSend                  = Icon("send")
  val IconSendOutline           = Icon("send outline")
  val IconShareAlternate        = Icon("share alternate")
  val IconShareAlternateSquare  = Icon("share alternate square")
  val IconShare                 = Icon("share")
  val IconShareSquare           = Icon("share square")
  val IconSignIn                = Icon("sign in")
  val IconSignOut               = Icon("sign out")
  val IconTheme                 = Icon("theme")
  val IconTranslate             = Icon("translate")
  val IconUndo                  = Icon("undo")
  val IconUnhide                = Icon("unhide")
  val IconUnlockAlternate       = Icon("unlock alternate")
  val IconUnlock                = Icon("unlock")
  val IconUpload                = Icon("upload")
  val IconWait                  = Icon("wait")
  val IconWizard                = Icon("wizard")
  val IconWrite                 = Icon("write")
  val IconWriteSquare           = Icon("write square")

  // Message Icon
  val IconAnnouncement  = Icon("announcement")
  val IconBirthday      = Icon("birthday")
  val IconHelp          = Icon("help")
  val IconHelpCircle    = Icon("help circle")
  val IconInfo          = Icon("info")
  val IconInfoCircle    = Icon("info circle")
  val IconWarning       = Icon("warning")
  val IconWarningCircle = Icon("warning circle")
  val IconWarningSign   = Icon("warning sign")

  // User types
  val IconChild    = Icon("child")
  val IconDoctor   = Icon("doctor")
  val IconHandicap = Icon("handicap")
  val IconSpy      = Icon("spy")
  val IconStudent  = Icon("student")
  val IconUser     = Icon("user")
  val IconUsers    = Icon("users")

  // Sexuality icons
  val IconFemale                = Icon("female")
  val IconGay                   = Icon("gay")
  val IconHeterosexual          = Icon("heterosexual")
  val IconIntergender           = Icon("intergender")
  val IconLesbian               = Icon("lesbian")
  val IconMale                  = Icon("male")
  val IconMan                   = Icon("man")
  val IconNeuter                = Icon("neuter")
  val IconNonBinaryTransgender  = Icon("non binary transgender")
  val IconTransgender           = Icon("transgender")
  val IconOtherGender           = Icon("other gender")
  val IconOtherGenderHorizontal = Icon("other gender horizontal")
  val IconOtherGenderVertical   = Icon("other gender vertical")
  val IconWoman                 = Icon("woman")

  // Layout icons
  val IconGridLayout       = Icon("grid layout")
  val IconListLayout       = Icon("list layout")
  val IconBlockLayout      = Icon("block layout")
  val IconZoom             = Icon("zoom")
  val IconZoomOut          = Icon("zoom out")
  val IconResizeVertical   = Icon("resize vertical")
  val IconResizeHorizontal = Icon("resize horizontal")
  val IconMaximize         = Icon("maximize")
  val IconCrop             = Icon("crop")

  // Object icons
  val IconAnchor           = Icon("anchor")
  val IconBar              = Icon("bar")
  val IconBomb             = Icon("bomb")
  val IconBook             = Icon("book")
  val IconBullseye         = Icon("bullseye")
  val IconCalculator       = Icon("calculator")
  val IconCheckeredFlag    = Icon("checkered flag")
  val IconCocktail         = Icon("cocktail")
  val IconDiamond          = Icon("diamond")
  val IconFax              = Icon("fax")
  val IconFireExtinguisher = Icon("fire extinguisher")
  val IconFire             = Icon("fire")
  val IconGift             = Icon("gift")
  val IconLeaf             = Icon("leaf")
  val IconLegal            = Icon("legal")
  val IconLemon            = Icon("lemon")
  val IconLifeRing         = Icon("life ring")
  val IconLightning        = Icon("lightning")
  val IconMagnet           = Icon("magnet")
  val IconMoney            = Icon("money")
  val IconMoon             = Icon("moon")
  val IconPlane            = Icon("plane")
  val IconPuzzle           = Icon("puzzle")
  val IconRain             = Icon("rain")
  val IconRoad             = Icon("road")
  val IconRocket           = Icon("rocket")
  val IconShipping         = Icon("shipping")
  val IconSoccer           = Icon("soccer")
  val IconSuitcase         = Icon("suitcase")
  val IconSun              = Icon("sun")
  val IconTravel           = Icon("travel")
  val IconTreatment        = Icon("treatment")
  val IconWorld            = Icon("world")

  // Shape Icons
  val IconAsterisk           = Icon("asterisk")
  val IconCertificate        = Icon("certificate")
  val IconCircle             = Icon("circle")
  val IconCircleNotched      = Icon("circle notched")
  val IconCircleThin         = Icon("circle thin")
  val IconCrosshairs         = Icon("crosshairs")
  val IconCube               = Icon("cube")
  val IconCubes              = Icon("cubes")
  val IconEllipsisHorizontal = Icon("ellipsis horizontal")
  val IconEllipsisVertical   = Icon("ellipsis vertical")
  val IconQuoteLeft          = Icon("quote left")
  val IconQuoteRight         = Icon("quote right")
  val IconSpinner            = Icon("spinner")
  val IconSquare             = Icon("square")
  val IconSquareOutline      = Icon("square outline")

  // selection icons
  val IconAddCircle           = Icon("add circle")
  val IconAddSquare           = Icon("add square")
  val IconCheckCircle         = Icon("check circle")
  val IconCheckCircleOutline  = Icon("check circle outline")
  val IconCheckSquare         = Icon("check square")
  val IconCheckmarkBox        = Icon("checkmark box")
  val IconCheckmark           = Icon("checkmark")
  val IconMinusCircle         = Icon("minus circle")
  val IconMinus               = Icon("minus")
  val IconMinusSquare         = Icon("minus square")
  val IconMinusSquareOutline  = Icon("minus square outline")
  val IconMove                = Icon("move")
  val IconPlus                = Icon("plus")
  val IconPlusSquareOutline   = Icon("plus square outline")
  val IconRadio               = Icon("radio")
  val IconRemoveCircle        = Icon("remove circle")
  val IconRemoveCircleOutline = Icon("remove circle outline")
  val IconRemove              = Icon("remove")
  val IconSelectedRadio       = Icon("selected radio")
  val IconToggleOff           = Icon("toggle off")
  val IconToggleOn            = Icon("toggle on")

  // Media icons
  val IconAreaChart   = Icon("area chart")
  val IconBarChart    = Icon("bar chart")
  val IconCameraRetro = Icon("camera retro")
  val IconNewspaper   = Icon("newspaper")
  val IconFilm        = Icon("film")
  val IconLineChart   = Icon("line chart")
  val IconPhoto       = Icon("photo")
  val IconPieChart    = Icon("pie chart")
  val IconSound       = Icon("sound")

  // pointers icons
  val IconAngleDoubleDown         = Icon("angle double down")
  val IconAngleDoubleLeft         = Icon("angle double left")
  val IconAngleDoubleRight        = Icon("angle double right")
  val IconAngleDoubleUp           = Icon("angle double up")
  val IconAngleDown               = Icon("angle down")
  val IconAngleLeft               = Icon("angle left")
  val IconAngleRight              = Icon("angle right")
  val IconAngleUp                 = Icon("angle up")
  val IconArrowCircleDown         = Icon("arrow circle down")
  val IconArrowCircleLeft         = Icon("arrow circle left")
  val IconArrowCircleOutlineDown  = Icon("arrow circle outline down")
  val IconArrowCircleOutlineLeft  = Icon("arrow circle outline left")
  val IconArrowCircleOutlineRight = Icon("arrow circle outline right")
  val IconArrowCircleOutlineUp    = Icon("arrow circle outline up")
  val IconArrowCircleRight        = Icon("arrow circle right")
  val IconArrowCircleUp           = Icon("arrow circle up")
  val IconArrowDown               = Icon("arrow down")
  val IconArrowLeft               = Icon("arrow left")
  val IconArrowRight              = Icon("arrow right")
  val IconArrowUp                 = Icon("arrow up")
  val IconCaretDown               = Icon("caret down")
  val IconCaretLeft               = Icon("caret left")
  val IconCaretRight              = Icon("caret right")
  val IconCaretUp                 = Icon("caret up")
  val IconChevronCircleDown       = Icon("chevron circle down")
  val IconChevronCircleLeft       = Icon("chevron circle left")
  val IconChevronCircleRight      = Icon("chevron circle right")
  val IconChevronCircleUp         = Icon("chevron circle up")
  val IconChevronDown             = Icon("chevron down")
  val IconChevronLeft             = Icon("chevron left")
  val IconChevronRight            = Icon("chevron right")
  val IconChevronUp               = Icon("chevron up")
  val IconLongArrowDown           = Icon("long arrow down")
  val IconLongArrowLeft           = Icon("long arrow left")
  val IconLongArrowRight          = Icon("long arrow right")
  val IconLongArrowUp             = Icon("long arrow up")
  val IconPointingDown            = Icon("pointing down")
  val IconPointingLeft            = Icon("pointing left")
  val IconPointingRight           = Icon("pointing right")
  val IconPointingUp              = Icon("pointing up")
  val IconToggleDown              = Icon("toggle down")
  val IconToggleLeft              = Icon("toggle left")
  val IconToggleRight             = Icon("toggle right")
  val IconToggleUp                = Icon("toggle up")

  // computer icons
  val IconDesktop               = Icon("desktop")
  val IconDiskOutline           = Icon("disk outline")
  val IconFileArchiveOutline    = Icon("file archive outline")
  val IconFileAudioOutline      = Icon("file audio outline")
  val IconFileCodeOutline       = Icon("file code outline")
  val IconFileExcelOutline      = Icon("file excel outline")
  val IconFile                  = Icon("file")
  val IconFileImageOutline      = Icon("file image outline")
  val IconFileOutline           = Icon("file outline")
  val IconFilePdfOutline        = Icon("file pdf outline")
  val IconFilePowerpointOutline = Icon("file powerpoint outline")
  val IconFileText              = Icon("file text")
  val IconFileTextOutline       = Icon("file text outline")
  val IconFileVideoOutline      = Icon("file video outline")
  val IconFileWordOutline       = Icon("file word outline")
  val IconFolder                = Icon("folder")
  val IconFolderOpen            = Icon("folder open")
  val IconFolderOpenOutline     = Icon("folder open outline")
  val IconFolderOutline         = Icon("folder outline")
  val IconGame                  = Icon("game")
  val IconKeyboard              = Icon("keyboard")
  val IconLaptop                = Icon("laptop")
  val IconLevelDown             = Icon("level down")
  val IconLevelUp               = Icon("level up")
  val IconMobile                = Icon("mobile")
  val IconPower                 = Icon("power")
  val IconPlug                  = Icon("plug")
  val IconTablet                = Icon("tablet")
  val IconTrash                 = Icon("trash")
  val IconTrashOutline          = Icon("trash outline")

  // technologies icon
  val IconBarcode   = Icon("barcode")
  val IconCss3      = Icon("css3")
  val IconDatabase  = Icon("database")
  val IconFork      = Icon("fork")
  val IconHtml5     = Icon("html5")
  val IconOpenid    = Icon("openid")
  val IconQrcode    = Icon("qrcode")
  val IconRSS       = Icon("rss")
  val IconRSSSquare = Icon("rss square")
  val IconServer    = Icon("server")

  // rating icons
  val IconEmptyHeart        = Icon("empty heart")
  val IconEmptyStar         = Icon("empty star")
  val IconFrown             = Icon("frown")
  val IconHeart             = Icon("heart")
  val IconMeh               = Icon("meh")
  val IconSmile             = Icon("smile")
  val IconStarHalfEmpty     = Icon("star half empty")
  val IconStarHalf          = Icon("star half")
  val IconStar              = Icon("star")
  val IconThumbsDown        = Icon("thumbs down")
  val IconThumbsOutlineDown = Icon("thumbs outline down")
  val IconThumbsOutlineUp   = Icon("thumbs outline up")
  val IconThumbsUp          = Icon("thumbs up")

  // audio icons
  val IconBackward          = Icon("backward")
  val IconEject             = Icon("eject")
  val IconFastBackward      = Icon("fast backward")
  val IconFastForward       = Icon("fast forward")
  val IconForward           = Icon("forward")
  val IconMusic             = Icon("music")
  val IconMute              = Icon("mute")
  val IconPause             = Icon("pause")
  val IconPlay              = Icon("play")
  val IconRecord            = Icon("record")
  val IconStepBackward      = Icon("step backward")
  val IconStepForward       = Icon("step forward")
  val IconStop              = Icon("stop")
  val IconStopCircle        = Icon("stop circle")
  val IconStopCircleOutline = Icon("stop circle outline")
  val IconUnmute            = Icon("unmute")
  val IconVideoPlay         = Icon("video play")
  val IconVideoPlayOutline  = Icon("video play outline")
  val IconVolumeDown        = Icon("volume down")
  val IconVolumeOff         = Icon("volume off")
  val IconVolumeUp          = Icon("volume up")

  // map icons
  val IconBuilding        = Icon("building")
  val IconBuildingOutline = Icon("building outline")
  val IconCar             = Icon("car")
  val IconCoffee          = Icon("coffee")
  val IconEmergency       = Icon("emergency")
  val IconFirstAid        = Icon("first aid")
  val IconFood            = Icon("food")
  val IconH               = Icon("h")
  val IconHospital        = Icon("hospital")
  val IconLocationArrow   = Icon("location arrow")
  val IconMarker          = Icon("marker")
  val IconMilitary        = Icon("military")
  val IconPaw             = Icon("paw")
  val IconSpaceShuttle    = Icon("space shuttle")
  val IconSpoon           = Icon("spoon")
  val IconTaxi            = Icon("taxi")
  val IconTree            = Icon("tree")
  val IconUniversity      = Icon("university")

  // tables
  val IconColumns                = Icon("columns")
  val IconSortAlphabetAscending  = Icon("sort alphabet ascending")
  val IconSortAlphabetDescending = Icon("sort alphabet descending")
  val IconSortAscending          = Icon("sort ascending")
  val IconSortContentAscending   = Icon("sort content ascending")
  val IconSortContentDescending  = Icon("sort content descending")
  val IconSortDescending         = Icon("sort descending")
  val IconSort                   = Icon("sort")
  val IconSortNumericAscending   = Icon("sort numeric ascending")
  val IconSortNumericDescending  = Icon("sort numeric descending")
  val IconTable                  = Icon("table")

  // text editor icons
  val IconAlignCenter   = Icon("align center")
  val IconAlignJustify  = Icon("align justify")
  val IconAlignLeft     = Icon("align left")
  val IconAlignRight    = Icon("align right")
  val IconAttach        = Icon("attach")
  val IconBold          = Icon("bold")
  val IconCopy          = Icon("copy")
  val IconCut           = Icon("cut")
  val IconFont          = Icon("font")
  val IconHeader        = Icon("header")
  val IconIndent        = Icon("indent")
  val IconItalic        = Icon("italic")
  val IconLinkify       = Icon("linkify")
  val IconList          = Icon("list")
  val IconOrderedList   = Icon("ordered list")
  val IconOutdent       = Icon("outdent")
  val IconParagraph     = Icon("paragraph")
  val IconPaste         = Icon("paste")
  val IconSave          = Icon("save")
  val IconStrikethrough = Icon("strikethrough")
  val IconSubscript     = Icon("subscript")
  val IconSuperscript   = Icon("superscript")
  val IconTextHeight    = Icon("text height")
  val IconTextWidth     = Icon("text width")
  val IconUnderline     = Icon("underline")
  val IconUnlink        = Icon("unlink")
  val IconUnorderedList = Icon("unordered list")

  // Currency icons
  val IconDollar = Icon("dollar")
  val IconEuro   = Icon("euro")
  val IconLira   = Icon("lira")
  val IconPound  = Icon("pound")
  val IconRuble  = Icon("ruble")
  val IconRupee  = Icon("rupee")
  val IconShekel = Icon("shekel")
  val IconWon    = Icon("won")
  val IconYen    = Icon("yen")

  // payment icons
  val IconAmericanExpress = Icon("american express")
  val IconDiscover        = Icon("discover")
  val IconGoogleWallet    = Icon("google wallet")
  val IconMastercard      = Icon("mastercard")
  val IconPaypalCard      = Icon("paypal card")
  val IconPaypal          = Icon("paypal")
  val IconStripe          = Icon("stripe")
  val IconVisa            = Icon("visa")

  // Brands
  val IconAdn                = Icon("adn")
  val IconAndroid            = Icon("android")
  val IconAngellist          = Icon("angellist")
  val IconApple              = Icon("apple")
  val IconBehance            = Icon("behance")
  val IconBehanceSquare      = Icon("behance square")
  val IconBitbucket          = Icon("bitbucket")
  val IconBitbucketSquare    = Icon("bitbucket square")
  val IconBitcoin            = Icon("bitcoin")
  val IconBuysellads         = Icon("buysellads")
  val IconCodepen            = Icon("codepen")
  val IconConnectdevelop     = Icon("connectdevelop")
  val IconDashcube           = Icon("dashcube")
  val IconDelicious          = Icon("delicious")
  val IconDeviantart         = Icon("deviantart")
  val IconDigg               = Icon("digg")
  val IconDribbble           = Icon("dribbble")
  val IconDropbox            = Icon("dropbox")
  val IconDrupal             = Icon("drupal")
  val IconEmpire             = Icon("empire")
  val IconFacebook           = Icon("facebook")
  val IconFacebookSquare     = Icon("facebook square")
  val IconFlickr             = Icon("flickr")
  val IconForumbee           = Icon("forumbee")
  val IconFoursquare         = Icon("foursquare")
  val IconGit                = Icon("git")
  val IconGitSquare          = Icon("git square")
  val IconGithubAlternate    = Icon("github alternate")
  val IconGithub             = Icon("github")
  val IconGithubSquare       = Icon("github square")
  val IconGittip             = Icon("gittip")
  val IconGoogle             = Icon("google")
  val IconGooglePlus         = Icon("google plus")
  val IconGooglePlusSquare   = Icon("google plus square")
  val IconHackerNews         = Icon("hacker news")
  val IconInstagram          = Icon("instagram")
  val IconIoxhost            = Icon("ioxhost")
  val IconJoomla             = Icon("joomla")
  val IconJsfiddle           = Icon("jsfiddle")
  val IconLastfm             = Icon("lastfm")
  val IconLastfmSquare       = Icon("lastfm square")
  val IconLeanpub            = Icon("leanpub")
  val IconLinkedin           = Icon("linkedin")
  val IconLinkedinSquare     = Icon("linkedin square")
  val IconLinux              = Icon("linux")
  val IconMaxcdn             = Icon("maxcdn")
  val IconMeanpath           = Icon("meanpath")
  val IconMedium             = Icon("medium")
  val IconPagelines          = Icon("pagelines")
  val IconPiedPiperAlternate = Icon("pied piper alternate")
  val IconPiedPiper          = Icon("pied piper")
  val IconPinterest          = Icon("pinterest")
  val IconPinterestSquare    = Icon("pinterest square")
  val IconQq                 = Icon("qq")
  val IconRebel              = Icon("rebel")
  val IconReddit             = Icon("reddit")
  val IconRedditSquare       = Icon("reddit square")
  val IconRenren             = Icon("renren")
  val IconSellsy             = Icon("sellsy")
  val IconShirtsinbulk       = Icon("shirtsinbulk")
  val IconSimplybuilt        = Icon("simplybuilt")
  val IconSkyatlas           = Icon("skyatlas")
  val IconSkype              = Icon("skype")
  val IconSlack              = Icon("slack")
  val IconSlideshare         = Icon("slideshare")
  val IconSoundcloud         = Icon("soundcloud")
  val IconSpotify            = Icon("spotify")
  val IconStackExchange      = Icon("stack exchange")
  val IconStackOverflow      = Icon("stack overflow")
  val IconSteam              = Icon("steam")
  val IconSteamSquare        = Icon("steam square")
  val IconStumbleuponCircle  = Icon("stumbleupon circle")
  val IconStumbleupon        = Icon("stumbleupon")
  val IconTencentWeibo       = Icon("tencent weibo")
  val IconTrello             = Icon("trello")
  val IconTumblr             = Icon("tumblr")
  val IconTumblrSquare       = Icon("tumblr square")
  val IconTwitch             = Icon("twitch")
  val IconTwitter            = Icon("twitter")
  val IconTwitterSquare      = Icon("twitter square")
  val IconViacoin            = Icon("viacoin")
  val IconVimeo              = Icon("vimeo")
  val IconVine               = Icon("vine")
  val IconVk                 = Icon("vk")
  val IconWechat             = Icon("wechat")
  val IconWeibo              = Icon("weibo")
  val IconWhatsapp           = Icon("whatsapp")
  val IconWindows            = Icon("windows")
  val IconWordpress          = Icon("wordpress")
  val IconXing               = Icon("xing")
  val IconXingSquare         = Icon("xing square")
  val IconYahoo              = Icon("yahoo")
  val IconYelp               = Icon("yelp")
  val IconYoutube            = Icon("youtube")
  val IconYoutubePlay        = Icon("youtube play")
  val IconYoutubeSquare      = Icon("youtube square")

  // Aliases
  val IconAttention = Icon("attention")
  val IconClose = Icon("close")

  sealed trait Flipped

  object Flipped {
    case object NotFlipped extends Flipped
    case object Horizontally extends Flipped
    case object Vertically extends Flipped
  }

  sealed trait Rotated

  object Rotated {
    case object NotRotated extends Rotated
    case object Clockwise extends Rotated
    case object CounterClockwise extends Rotated
  }

  case class Props(id: String,
                   disabled: Boolean = false,
                   loading: Boolean = false,
                   fitted: Boolean = false,
                   size: Size = Size.NotSized,
                   link: Boolean = false,
                   flipped: Flipped = Flipped.NotFlipped,
                   rotated: Rotated = Rotated.NotRotated,
                   circular: Boolean = false,
                   bordered: Boolean = false,
                   inverted: Boolean = false,
                   color: Option[String] = None,
                   extraStyles: List[scalacss.internal.StyleA] = Nil,
                   key: String = "",
                   onClick: Callback = Callback.empty)

  // Used to call Icon directly on a jsx component declaration
  implicit def icon2TagMod(i: Icon): VdomElement = i.component

  def apply(s: String, children: VdomNode*): Icon = Icon(Props(s), children)
}
