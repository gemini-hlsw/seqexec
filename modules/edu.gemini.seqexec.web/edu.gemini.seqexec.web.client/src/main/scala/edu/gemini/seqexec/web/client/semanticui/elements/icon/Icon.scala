// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.semanticui.elements.icon

import edu.gemini.seqexec.web.client.semanticui.Size
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import scalacss.ScalaCssReact._

import scalaz.syntax.equal._
import scalaz.Equal

/**
  * Semantic UI Icon component
  */
final case class Icon(p: Icon.Props, children: Seq[VdomNode]) {
  import Icon._

  // Custom copy constructor to avoid passing the id again
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
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
          "tiny"                     -> (p.size === Size.Tiny),
          "mini"                     -> (p.size === Size.Mini),
          "small"                    -> (p.size === Size.Small),
          "large"                    -> (p.size === Size.Large),
          "big"                      -> (p.size === Size.Big),
          "huge"                     -> (p.size === Size.Huge),
          "massive"                  -> (p.size === Size.Massive),
          "link"                     -> p.link,
          "horizontally flipped"     -> (p.flipped === Flipped.Horizontally),
          "vertically flipped"       -> (p.flipped === Flipped.Vertically),
          "clockwise rotated"        -> (p.rotated === Rotated.Clockwise),
          "counterclockwise rotated" -> (p.rotated === Rotated.CounterClockwise),
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
  lazy val IconAlarm: Icon             = Icon("alarm")
  lazy val IconAlarmSlash: Icon        = Icon("alarm slash")
  lazy val IconAlarmOutline: Icon      = Icon("alarm outline")
  lazy val IconAlarmSlashOutline: Icon = Icon("alarm slash outline")
  lazy val IconAt: Icon                = Icon("at")
  lazy val IconBrowser: Icon           = Icon("browser")
  lazy val IconBug: Icon               = Icon("bug")
  lazy val IconCalendarOutline: Icon   = Icon("calendar outline")
  lazy val IconCalendar: Icon          = Icon("calendar")
  lazy val IconCloud: Icon             = Icon("cloud")
  lazy val IconCode: Icon              = Icon("code")
  lazy val IconComment: Icon           = Icon("comment")
  lazy val IconComments: Icon          = Icon("comments")
  lazy val IconCommentOutline: Icon    = Icon("comment outline")
  lazy val IconCommentsOutline: Icon   = Icon("comments outline")
  lazy val IconCopyright: Icon         = Icon("copyright")
  lazy val IconDashboard: Icon         = Icon("dashboard")
  lazy val IconDropdown: Icon          = Icon("dropdown")
  lazy val IconExternalSquare: Icon    = Icon("external square")
  lazy val IconExternal: Icon          = Icon("external")
  lazy val IconEyedropper: Icon        = Icon("eyedropper")
  lazy val IconFeed: Icon              = Icon("feed")
  lazy val IconFind: Icon              = Icon("find")
  lazy val IconHeartbeat: Icon         = Icon("heartbeat")
  lazy val IconHistory: Icon           = Icon("history")
  lazy val IconHome: Icon              = Icon("home")
  lazy val IconIdea: Icon              = Icon("idea")
  lazy val IconInbox: Icon             = Icon("inbox")
  lazy val IconLab: Icon               = Icon("lab")
  lazy val IconMail: Icon              = Icon("mail")
  lazy val IconMailOutline: Icon       = Icon("mail outline")
  lazy val IconMailSquare: Icon        = Icon("mail square")
  lazy val IconMap: Icon               = Icon("map")
  lazy val IconOptions: Icon           = Icon("options")
  lazy val IconPaintBrush: Icon        = Icon("paint brush")
  lazy val IconPayment: Icon           = Icon("payment")
  lazy val IconPhone: Icon             = Icon("phone")
  lazy val IconPhoneSquare: Icon       = Icon("phone square")
  lazy val IconPrivacy: Icon           = Icon("privacy")
  lazy val IconProtect: Icon           = Icon("protect")
  lazy val IconSearch: Icon            = Icon("search")
  lazy val IconSetting: Icon           = Icon("setting")
  lazy val IconSettings: Icon          = Icon("settings")
  lazy val IconShop: Icon              = Icon("shop")
  lazy val IconSidebar: Icon           = Icon("sidebar")
  lazy val IconSignal: Icon            = Icon("signal")
  lazy val IconSitemap: Icon           = Icon("sitemap")
  lazy val IconTag: Icon               = Icon("tag")
  lazy val IconTags: Icon              = Icon("tags")
  lazy val IconTasks: Icon             = Icon("tasks")
  lazy val IconTerminal: Icon          = Icon("terminal")
  lazy val IconTextTelephone: Icon     = Icon("text telephone")
  lazy val IconTicket: Icon            = Icon("ticket")
  lazy val IconTrophy: Icon            = Icon("trophy")
  lazy val IconWifi: Icon              = Icon("wifi")

  // User action icons
  lazy val IconAdjust: Icon                = Icon("adjust")
  lazy val IconAddUser: Icon               = Icon("add user")
  lazy val IconAddToCart: Icon             = Icon("add to cart")
  lazy val IconArchive: Icon               = Icon("archive")
  lazy val IconBan: Icon                   = Icon("ban")
  lazy val IconBookmark: Icon              = Icon("bookmark")
  lazy val IconCall: Icon                  = Icon("call")
  lazy val IconCallSquare: Icon            = Icon("call square")
  lazy val IconCloudDownload: Icon         = Icon("cloud download")
  lazy val IconCloudUpload: Icon           = Icon("cloud upload")
  lazy val IconCompress: Icon              = Icon("compress")
  lazy val IconConfigure: Icon             = Icon("configure")
  lazy val IconDownload: Icon              = Icon("download")
  lazy val IconEdit: Icon                  = Icon("edit")
  lazy val IconErase: Icon                 = Icon("erase")
  lazy val IconExchange: Icon              = Icon("exchange")
  lazy val IconExternalShare: Icon         = Icon("external share")
  lazy val IconExpand: Icon                = Icon("expand")
  lazy val IconFilter: Icon                = Icon("filter")
  lazy val IconFlag: Icon                  = Icon("flag")
  lazy val IconFlagOutline: Icon           = Icon("flag outline")
  lazy val IconForwardMail: Icon           = Icon("forward mail")
  lazy val IconHide: Icon                  = Icon("hide")
  lazy val IconInCart: Icon                = Icon("in cart")
  lazy val IconLock: Icon                  = Icon("lock")
  lazy val IconPin: Icon                   = Icon("pin")
  lazy val IconPrint: Icon                 = Icon("print")
  lazy val IconRandom: Icon                = Icon("random")
  lazy val IconRecycle: Icon               = Icon("recycle")
  lazy val IconRefresh: Icon               = Icon("refresh")
  lazy val IconRemoveBookmark: Icon        = Icon("remove bookmark")
  lazy val IconRemoveUser: Icon            = Icon("remove user")
  lazy val IconRepeat: Icon                = Icon("repeat")
  lazy val IconReplyAll: Icon              = Icon("reply all")
  lazy val IconReply: Icon                 = Icon("reply")
  lazy val IconRetweet: Icon               = Icon("retweet")
  lazy val IconSend: Icon                  = Icon("send")
  lazy val IconSendOutline: Icon           = Icon("send outline")
  lazy val IconShareAlternate: Icon        = Icon("share alternate")
  lazy val IconShareAlternateSquare: Icon  = Icon("share alternate square")
  lazy val IconShare: Icon                 = Icon("share")
  lazy val IconShareSquare: Icon           = Icon("share square")
  lazy val IconSignIn: Icon                = Icon("sign in")
  lazy val IconSignOut: Icon               = Icon("sign out")
  lazy val IconTheme: Icon                 = Icon("theme")
  lazy val IconTranslate: Icon             = Icon("translate")
  lazy val IconUndo: Icon                  = Icon("undo")
  lazy val IconUnhide: Icon                = Icon("unhide")
  lazy val IconUnlockAlternate: Icon       = Icon("unlock alternate")
  lazy val IconUnlock: Icon                = Icon("unlock")
  lazy val IconUpload: Icon                = Icon("upload")
  lazy val IconWait: Icon                  = Icon("wait")
  lazy val IconWizard: Icon                = Icon("wizard")
  lazy val IconWrite: Icon                 = Icon("write")
  lazy val IconWriteSquare: Icon           = Icon("write square")

  // Message Icon
  lazy val IconAnnouncement: Icon  = Icon("announcement")
  lazy val IconBirthday: Icon      = Icon("birthday")
  lazy val IconHelp: Icon          = Icon("help")
  lazy val IconHelpCircle: Icon    = Icon("help circle")
  lazy val IconInfo: Icon          = Icon("info")
  lazy val IconInfoCircle: Icon    = Icon("info circle")
  lazy val IconWarning: Icon       = Icon("warning")
  lazy val IconWarningCircle: Icon = Icon("warning circle")
  lazy val IconWarningSign: Icon   = Icon("warning sign")

  // User types
  lazy val IconChild: Icon    = Icon("child")
  lazy val IconDoctor: Icon   = Icon("doctor")
  lazy val IconHandicap: Icon = Icon("handicap")
  lazy val IconSpy: Icon      = Icon("spy")
  lazy val IconStudent: Icon  = Icon("student")
  lazy val IconUser: Icon     = Icon("user")
  lazy val IconUsers: Icon    = Icon("users")

  // Sexuality icons
  lazy val IconFemale: Icon                = Icon("female")
  lazy val IconGay: Icon                   = Icon("gay")
  lazy val IconHeterosexual: Icon          = Icon("heterosexual")
  lazy val IconIntergender: Icon           = Icon("intergender")
  lazy val IconLesbian: Icon               = Icon("lesbian")
  lazy val IconMale: Icon                  = Icon("male")
  lazy val IconMan: Icon                   = Icon("man")
  lazy val IconNeuter: Icon                = Icon("neuter")
  lazy val IconNonBinaryTransgender: Icon  = Icon("non binary transgender")
  lazy val IconTransgender: Icon           = Icon("transgender")
  lazy val IconOtherGender: Icon           = Icon("other gender")
  lazy val IconOtherGenderHorizontal: Icon = Icon("other gender horizontal")
  lazy val IconOtherGenderVertical: Icon   = Icon("other gender vertical")
  lazy val IconWoman: Icon                 = Icon("woman")

  // Layout icons
  lazy val IconGridLayout: Icon       = Icon("grid layout")
  lazy val IconListLayout: Icon       = Icon("list layout")
  lazy val IconBlockLayout: Icon      = Icon("block layout")
  lazy val IconZoom: Icon             = Icon("zoom")
  lazy val IconZoomOut: Icon          = Icon("zoom out")
  lazy val IconResizeVertical: Icon   = Icon("resize vertical")
  lazy val IconResizeHorizontal: Icon = Icon("resize horizontal")
  lazy val IconMaximize: Icon         = Icon("maximize")
  lazy val IconCrop: Icon             = Icon("crop")

  // Object icons
  lazy val IconAnchor: Icon           = Icon("anchor")
  lazy val IconBar: Icon              = Icon("bar")
  lazy val IconBomb: Icon             = Icon("bomb")
  lazy val IconBook: Icon             = Icon("book")
  lazy val IconBullseye: Icon         = Icon("bullseye")
  lazy val IconCalculator: Icon       = Icon("calculator")
  lazy val IconCheckeredFlag: Icon    = Icon("checkered flag")
  lazy val IconCocktail: Icon         = Icon("cocktail")
  lazy val IconDiamond: Icon          = Icon("diamond")
  lazy val IconFax: Icon              = Icon("fax")
  lazy val IconFireExtinguisher: Icon = Icon("fire extinguisher")
  lazy val IconFire: Icon             = Icon("fire")
  lazy val IconGift: Icon             = Icon("gift")
  lazy val IconLeaf: Icon             = Icon("leaf")
  lazy val IconLegal: Icon            = Icon("legal")
  lazy val IconLemon: Icon            = Icon("lemon")
  lazy val IconLifeRing: Icon         = Icon("life ring")
  lazy val IconLightning: Icon        = Icon("lightning")
  lazy val IconMagnet: Icon           = Icon("magnet")
  lazy val IconMoney: Icon            = Icon("money")
  lazy val IconMoon: Icon             = Icon("moon")
  lazy val IconPlane: Icon            = Icon("plane")
  lazy val IconPuzzle: Icon           = Icon("puzzle")
  lazy val IconRain: Icon             = Icon("rain")
  lazy val IconRoad: Icon             = Icon("road")
  lazy val IconRocket: Icon           = Icon("rocket")
  lazy val IconShipping: Icon         = Icon("shipping")
  lazy val IconSoccer: Icon           = Icon("soccer")
  lazy val IconSuitcase: Icon         = Icon("suitcase")
  lazy val IconSun: Icon              = Icon("sun")
  lazy val IconTravel: Icon           = Icon("travel")
  lazy val IconTreatment: Icon        = Icon("treatment")
  lazy val IconWorld: Icon            = Icon("world")

  // Shape Icons
  lazy val IconAsterisk: Icon           = Icon("asterisk")
  lazy val IconCertificate: Icon        = Icon("certificate")
  lazy val IconCircle: Icon             = Icon("circle")
  lazy val IconCircleNotched: Icon      = Icon("circle notched")
  lazy val IconCircleThin: Icon         = Icon("circle thin")
  lazy val IconCrosshairs: Icon         = Icon("crosshairs")
  lazy val IconCube: Icon               = Icon("cube")
  lazy val IconCubes: Icon              = Icon("cubes")
  lazy val IconEllipsisHorizontal: Icon = Icon("ellipsis horizontal")
  lazy val IconEllipsisVertical: Icon   = Icon("ellipsis vertical")
  lazy val IconQuoteLeft: Icon          = Icon("quote left")
  lazy val IconQuoteRight: Icon         = Icon("quote right")
  lazy val IconSpinner: Icon            = Icon("spinner")
  lazy val IconSquare: Icon             = Icon("square")
  lazy val IconSquareOutline: Icon      = Icon("square outline")

  // selection icons
  lazy val IconAddCircle: Icon           = Icon("add circle")
  lazy val IconAddSquare: Icon           = Icon("add square")
  lazy val IconCheckCircle: Icon         = Icon("check circle")
  lazy val IconCheckCircleOutline: Icon  = Icon("check circle outline")
  lazy val IconCheckSquare: Icon         = Icon("check square")
  lazy val IconCheckmarkBox: Icon        = Icon("checkmark box")
  lazy val IconCheckmark: Icon           = Icon("checkmark")
  lazy val IconMinusCircle: Icon         = Icon("minus circle")
  lazy val IconMinus: Icon               = Icon("minus")
  lazy val IconMinusSquare: Icon         = Icon("minus square")
  lazy val IconMinusSquareOutline: Icon  = Icon("minus square outline")
  lazy val IconMove: Icon                = Icon("move")
  lazy val IconPlus: Icon                = Icon("plus")
  lazy val IconPlusSquareOutline: Icon   = Icon("plus square outline")
  lazy val IconRadio: Icon               = Icon("radio")
  lazy val IconRemoveCircle: Icon        = Icon("remove circle")
  lazy val IconRemoveCircleOutline: Icon = Icon("remove circle outline")
  lazy val IconRemove: Icon              = Icon("remove")
  lazy val IconSelectedRadio: Icon       = Icon("selected radio")
  lazy val IconToggleOff: Icon           = Icon("toggle off")
  lazy val IconToggleOn: Icon            = Icon("toggle on")

  // Media icons
  lazy val IconAreaChart: Icon   = Icon("area chart")
  lazy val IconBarChart: Icon    = Icon("bar chart")
  lazy val IconCameraRetro: Icon = Icon("camera retro")
  lazy val IconNewspaper: Icon   = Icon("newspaper")
  lazy val IconFilm: Icon        = Icon("film")
  lazy val IconLineChart: Icon   = Icon("line chart")
  lazy val IconPhoto: Icon       = Icon("photo")
  lazy val IconPieChart: Icon    = Icon("pie chart")
  lazy val IconSound: Icon       = Icon("sound")

  // pointers icons
  lazy val IconAngleDoubleDown: Icon         = Icon("angle double down")
  lazy val IconAngleDoubleLeft: Icon         = Icon("angle double left")
  lazy val IconAngleDoubleRight: Icon        = Icon("angle double right")
  lazy val IconAngleDoubleUp: Icon           = Icon("angle double up")
  lazy val IconAngleDown: Icon               = Icon("angle down")
  lazy val IconAngleLeft: Icon               = Icon("angle left")
  lazy val IconAngleRight: Icon              = Icon("angle right")
  lazy val IconAngleUp: Icon                 = Icon("angle up")
  lazy val IconArrowCircleDown: Icon         = Icon("arrow circle down")
  lazy val IconArrowCircleLeft: Icon         = Icon("arrow circle left")
  lazy val IconArrowCircleOutlineDown: Icon  = Icon("arrow circle outline down")
  lazy val IconArrowCircleOutlineLeft: Icon  = Icon("arrow circle outline left")
  lazy val IconArrowCircleOutlineRight: Icon = Icon("arrow circle outline right")
  lazy val IconArrowCircleOutlineUp: Icon    = Icon("arrow circle outline up")
  lazy val IconArrowCircleRight: Icon        = Icon("arrow circle right")
  lazy val IconArrowCircleUp: Icon           = Icon("arrow circle up")
  lazy val IconArrowDown: Icon               = Icon("arrow down")
  lazy val IconArrowLeft: Icon               = Icon("arrow left")
  lazy val IconArrowRight: Icon              = Icon("arrow right")
  lazy val IconArrowUp: Icon                 = Icon("arrow up")
  lazy val IconCaretDown: Icon               = Icon("caret down")
  lazy val IconCaretLeft: Icon               = Icon("caret left")
  lazy val IconCaretRight: Icon              = Icon("caret right")
  lazy val IconCaretUp: Icon                 = Icon("caret up")
  lazy val IconChevronCircleDown: Icon       = Icon("chevron circle down")
  lazy val IconChevronCircleLeft: Icon       = Icon("chevron circle left")
  lazy val IconChevronCircleRight: Icon      = Icon("chevron circle right")
  lazy val IconChevronCircleUp: Icon         = Icon("chevron circle up")
  lazy val IconChevronDown: Icon             = Icon("chevron down")
  lazy val IconChevronLeft: Icon             = Icon("chevron left")
  lazy val IconChevronRight: Icon            = Icon("chevron right")
  lazy val IconChevronUp: Icon               = Icon("chevron up")
  lazy val IconLongArrowDown: Icon           = Icon("long arrow down")
  lazy val IconLongArrowLeft: Icon           = Icon("long arrow left")
  lazy val IconLongArrowRight: Icon          = Icon("long arrow right")
  lazy val IconLongArrowUp: Icon             = Icon("long arrow up")
  lazy val IconPointingDown: Icon            = Icon("pointing down")
  lazy val IconPointingLeft: Icon            = Icon("pointing left")
  lazy val IconPointingRight: Icon           = Icon("pointing right")
  lazy val IconPointingUp: Icon              = Icon("pointing up")
  lazy val IconToggleDown: Icon              = Icon("toggle down")
  lazy val IconToggleLeft: Icon              = Icon("toggle left")
  lazy val IconToggleRight: Icon             = Icon("toggle right")
  lazy val IconToggleUp: Icon                = Icon("toggle up")

  // computer icons
  lazy val IconDesktop: Icon               = Icon("desktop")
  lazy val IconDiskOutline: Icon           = Icon("disk outline")
  lazy val IconFileArchiveOutline: Icon    = Icon("file archive outline")
  lazy val IconFileAudioOutline: Icon      = Icon("file audio outline")
  lazy val IconFileCodeOutline: Icon       = Icon("file code outline")
  lazy val IconFileExcelOutline: Icon      = Icon("file excel outline")
  lazy val IconFile: Icon                  = Icon("file")
  lazy val IconFileImageOutline: Icon      = Icon("file image outline")
  lazy val IconFileOutline: Icon           = Icon("file outline")
  lazy val IconFilePdfOutline: Icon        = Icon("file pdf outline")
  lazy val IconFilePowerpointOutline: Icon = Icon("file powerpoint outline")
  lazy val IconFileText: Icon              = Icon("file text")
  lazy val IconFileTextOutline: Icon       = Icon("file text outline")
  lazy val IconFileVideoOutline: Icon      = Icon("file video outline")
  lazy val IconFileWordOutline: Icon       = Icon("file word outline")
  lazy val IconFolder: Icon                = Icon("folder")
  lazy val IconFolderOpen: Icon            = Icon("folder open")
  lazy val IconFolderOpenOutline: Icon     = Icon("folder open outline")
  lazy val IconFolderOutline: Icon         = Icon("folder outline")
  lazy val IconGame: Icon                  = Icon("game")
  lazy val IconKeyboard: Icon              = Icon("keyboard")
  lazy val IconLaptop: Icon                = Icon("laptop")
  lazy val IconLevelDown: Icon             = Icon("level down")
  lazy val IconLevelUp: Icon               = Icon("level up")
  lazy val IconMobile: Icon                = Icon("mobile")
  lazy val IconPower: Icon                 = Icon("power")
  lazy val IconPlug: Icon                  = Icon("plug")
  lazy val IconTablet: Icon                = Icon("tablet")
  lazy val IconTrash: Icon                 = Icon("trash")
  lazy val IconTrashOutline: Icon          = Icon("trash outline")

  // technologies icon
  lazy val IconBarcode: Icon   = Icon("barcode")
  lazy val IconCss3: Icon      = Icon("css3")
  lazy val IconDatabase: Icon  = Icon("database")
  lazy val IconFork: Icon      = Icon("fork")
  lazy val IconHtml5: Icon     = Icon("html5")
  lazy val IconOpenid: Icon    = Icon("openid")
  lazy val IconQrcode: Icon    = Icon("qrcode")
  lazy val IconRSS: Icon       = Icon("rss")
  lazy val IconRSSSquare: Icon = Icon("rss square")
  lazy val IconServer: Icon    = Icon("server")

  // rating icons
  lazy val IconEmptyHeart: Icon        = Icon("empty heart")
  lazy val IconEmptyStar: Icon         = Icon("empty star")
  lazy val IconFrown: Icon             = Icon("frown")
  lazy val IconHeart: Icon             = Icon("heart")
  lazy val IconMeh: Icon               = Icon("meh")
  lazy val IconSmile: Icon             = Icon("smile")
  lazy val IconStarHalfEmpty: Icon     = Icon("star half empty")
  lazy val IconStarHalf: Icon          = Icon("star half")
  lazy val IconStar: Icon              = Icon("star")
  lazy val IconThumbsDown: Icon        = Icon("thumbs down")
  lazy val IconThumbsOutlineDown: Icon = Icon("thumbs outline down")
  lazy val IconThumbsOutlineUp: Icon   = Icon("thumbs outline up")
  lazy val IconThumbsUp: Icon          = Icon("thumbs up")

  // audio icons
  lazy val IconBackward: Icon          = Icon("backward")
  lazy val IconEject: Icon             = Icon("eject")
  lazy val IconFastBackward: Icon      = Icon("fast backward")
  lazy val IconFastForward: Icon       = Icon("fast forward")
  lazy val IconForward: Icon           = Icon("forward")
  lazy val IconMusic: Icon             = Icon("music")
  lazy val IconMute: Icon              = Icon("mute")
  lazy val IconPause: Icon             = Icon("pause")
  lazy val IconPlay: Icon              = Icon("play")
  lazy val IconRecord: Icon            = Icon("record")
  lazy val IconStepBackward: Icon      = Icon("step backward")
  lazy val IconStepForward: Icon       = Icon("step forward")
  lazy val IconStop: Icon              = Icon("stop")
  lazy val IconStopCircle: Icon        = Icon("stop circle")
  lazy val IconStopCircleOutline: Icon = Icon("stop circle outline")
  lazy val IconUnmute: Icon            = Icon("unmute")
  lazy val IconVideoPlay: Icon         = Icon("video play")
  lazy val IconVideoPlayOutline: Icon  = Icon("video play outline")
  lazy val IconVolumeDown: Icon        = Icon("volume down")
  lazy val IconVolumeOff: Icon         = Icon("volume off")
  lazy val IconVolumeUp: Icon          = Icon("volume up")

  // map icons
  lazy val IconBuilding: Icon        = Icon("building")
  lazy val IconBuildingOutline: Icon = Icon("building outline")
  lazy val IconCar: Icon             = Icon("car")
  lazy val IconCoffee: Icon          = Icon("coffee")
  lazy val IconEmergency: Icon       = Icon("emergency")
  lazy val IconFirstAid: Icon        = Icon("first aid")
  lazy val IconFood: Icon            = Icon("food")
  lazy val IconH: Icon               = Icon("h")
  lazy val IconHospital: Icon        = Icon("hospital")
  lazy val IconLocationArrow: Icon   = Icon("location arrow")
  lazy val IconMarker: Icon          = Icon("marker")
  lazy val IconMilitary: Icon        = Icon("military")
  lazy val IconPaw: Icon             = Icon("paw")
  lazy val IconSpaceShuttle: Icon    = Icon("space shuttle")
  lazy val IconSpoon: Icon           = Icon("spoon")
  lazy val IconTaxi: Icon            = Icon("taxi")
  lazy val IconTree: Icon            = Icon("tree")
  lazy val IconUniversity: Icon      = Icon("university")

  // tables
  lazy val IconColumns: Icon                = Icon("columns")
  lazy val IconSortAlphabetAscending: Icon  = Icon("sort alphabet ascending")
  lazy val IconSortAlphabetDescending: Icon = Icon("sort alphabet descending")
  lazy val IconSortAscending: Icon          = Icon("sort ascending")
  lazy val IconSortContentAscending: Icon   = Icon("sort content ascending")
  lazy val IconSortContentDescending: Icon  = Icon("sort content descending")
  lazy val IconSortDescending: Icon         = Icon("sort descending")
  lazy val IconSort: Icon                   = Icon("sort")
  lazy val IconSortNumericAscending: Icon   = Icon("sort numeric ascending")
  lazy val IconSortNumericDescending: Icon  = Icon("sort numeric descending")
  lazy val IconTable: Icon                  = Icon("table")

  // text editor icons
  lazy val IconAlignCenter: Icon   = Icon("align center")
  lazy val IconAlignJustify: Icon  = Icon("align justify")
  lazy val IconAlignLeft: Icon     = Icon("align left")
  lazy val IconAlignRight: Icon    = Icon("align right")
  lazy val IconAttach: Icon        = Icon("attach")
  lazy val IconBold: Icon          = Icon("bold")
  lazy val IconCopy: Icon          = Icon("copy")
  lazy val IconCut: Icon           = Icon("cut")
  lazy val IconFont: Icon          = Icon("font")
  lazy val IconHeader: Icon        = Icon("header")
  lazy val IconIndent: Icon        = Icon("indent")
  lazy val IconItalic: Icon        = Icon("italic")
  lazy val IconLinkify: Icon       = Icon("linkify")
  lazy val IconList: Icon          = Icon("list")
  lazy val IconOrderedList: Icon   = Icon("ordered list")
  lazy val IconOutdent: Icon       = Icon("outdent")
  lazy val IconParagraph: Icon     = Icon("paragraph")
  lazy val IconPaste: Icon         = Icon("paste")
  lazy val IconSave: Icon          = Icon("save")
  lazy val IconStrikethrough: Icon = Icon("strikethrough")
  lazy val IconSubscript: Icon     = Icon("subscript")
  lazy val IconSuperscript: Icon   = Icon("superscript")
  lazy val IconTextHeight: Icon    = Icon("text height")
  lazy val IconTextWidth: Icon     = Icon("text width")
  lazy val IconUnderline: Icon     = Icon("underline")
  lazy val IconUnlink: Icon        = Icon("unlink")
  lazy val IconUnorderedList: Icon = Icon("unordered list")

  // Currency icons
  lazy val IconDollar: Icon = Icon("dollar")
  lazy val IconEuro: Icon   = Icon("euro")
  lazy val IconLira: Icon   = Icon("lira")
  lazy val IconPound: Icon  = Icon("pound")
  lazy val IconRuble: Icon  = Icon("ruble")
  lazy val IconRupee: Icon  = Icon("rupee")
  lazy val IconShekel: Icon = Icon("shekel")
  lazy val IconWon: Icon    = Icon("won")
  lazy val IconYen: Icon    = Icon("yen")

  // payment icons
  lazy val IconAmericanExpress: Icon = Icon("american express")
  lazy val IconDiscover: Icon        = Icon("discover")
  lazy val IconGoogleWallet: Icon    = Icon("google wallet")
  lazy val IconMastercard: Icon      = Icon("mastercard")
  lazy val IconPaypalCard: Icon      = Icon("paypal card")
  lazy val IconPaypal: Icon          = Icon("paypal")
  lazy val IconStripe: Icon          = Icon("stripe")
  lazy val IconVisa: Icon            = Icon("visa")

  // Brands
  lazy val IconAdn: Icon                = Icon("adn")
  lazy val IconAndroid: Icon            = Icon("android")
  lazy val IconAngellist: Icon          = Icon("angellist")
  lazy val IconApple: Icon              = Icon("apple")
  lazy val IconBehance: Icon            = Icon("behance")
  lazy val IconBehanceSquare: Icon      = Icon("behance square")
  lazy val IconBitbucket: Icon          = Icon("bitbucket")
  lazy val IconBitbucketSquare: Icon    = Icon("bitbucket square")
  lazy val IconBitcoin: Icon            = Icon("bitcoin")
  lazy val IconBuysellads: Icon         = Icon("buysellads")
  lazy val IconCodepen: Icon            = Icon("codepen")
  lazy val IconConnectdevelop: Icon     = Icon("connectdevelop")
  lazy val IconDashcube: Icon           = Icon("dashcube")
  lazy val IconDelicious: Icon          = Icon("delicious")
  lazy val IconDeviantart: Icon         = Icon("deviantart")
  lazy val IconDigg: Icon               = Icon("digg")
  lazy val IconDribbble: Icon           = Icon("dribbble")
  lazy val IconDropbox: Icon            = Icon("dropbox")
  lazy val IconDrupal: Icon             = Icon("drupal")
  lazy val IconEmpire: Icon             = Icon("empire")
  lazy val IconFacebook: Icon           = Icon("facebook")
  lazy val IconFacebookSquare: Icon     = Icon("facebook square")
  lazy val IconFlickr: Icon             = Icon("flickr")
  lazy val IconForumbee: Icon           = Icon("forumbee")
  lazy val IconFoursquare: Icon         = Icon("foursquare")
  lazy val IconGit: Icon                = Icon("git")
  lazy val IconGitSquare: Icon          = Icon("git square")
  lazy val IconGithubAlternate: Icon    = Icon("github alternate")
  lazy val IconGithub: Icon             = Icon("github")
  lazy val IconGithubSquare: Icon       = Icon("github square")
  lazy val IconGittip: Icon             = Icon("gittip")
  lazy val IconGoogle: Icon             = Icon("google")
  lazy val IconGooglePlus: Icon         = Icon("google plus")
  lazy val IconGooglePlusSquare: Icon   = Icon("google plus square")
  lazy val IconHackerNews: Icon         = Icon("hacker news")
  lazy val IconInstagram: Icon          = Icon("instagram")
  lazy val IconIoxhost: Icon            = Icon("ioxhost")
  lazy val IconJoomla: Icon             = Icon("joomla")
  lazy val IconJsfiddle: Icon           = Icon("jsfiddle")
  lazy val IconLastfm: Icon             = Icon("lastfm")
  lazy val IconLastfmSquare: Icon       = Icon("lastfm square")
  lazy val IconLeanpub: Icon            = Icon("leanpub")
  lazy val IconLinkedin: Icon           = Icon("linkedin")
  lazy val IconLinkedinSquare: Icon     = Icon("linkedin square")
  lazy val IconLinux: Icon              = Icon("linux")
  lazy val IconMaxcdn: Icon             = Icon("maxcdn")
  lazy val IconMeanpath: Icon           = Icon("meanpath")
  lazy val IconMedium: Icon             = Icon("medium")
  lazy val IconPagelines: Icon          = Icon("pagelines")
  lazy val IconPiedPiperAlternate: Icon = Icon("pied piper alternate")
  lazy val IconPiedPiper: Icon          = Icon("pied piper")
  lazy val IconPinterest: Icon          = Icon("pinterest")
  lazy val IconPinterestSquare: Icon    = Icon("pinterest square")
  lazy val IconQq: Icon                 = Icon("qq")
  lazy val IconRebel: Icon              = Icon("rebel")
  lazy val IconReddit: Icon             = Icon("reddit")
  lazy val IconRedditSquare: Icon       = Icon("reddit square")
  lazy val IconRenren: Icon             = Icon("renren")
  lazy val IconSellsy: Icon             = Icon("sellsy")
  lazy val IconShirtsinbulk: Icon       = Icon("shirtsinbulk")
  lazy val IconSimplybuilt: Icon        = Icon("simplybuilt")
  lazy val IconSkyatlas: Icon           = Icon("skyatlas")
  lazy val IconSkype: Icon              = Icon("skype")
  lazy val IconSlack: Icon              = Icon("slack")
  lazy val IconSlideshare: Icon         = Icon("slideshare")
  lazy val IconSoundcloud: Icon         = Icon("soundcloud")
  lazy val IconSpotify: Icon            = Icon("spotify")
  lazy val IconStackExchange: Icon      = Icon("stack exchange")
  lazy val IconStackOverflow: Icon      = Icon("stack overflow")
  lazy val IconSteam: Icon              = Icon("steam")
  lazy val IconSteamSquare: Icon        = Icon("steam square")
  lazy val IconStumbleuponCircle: Icon  = Icon("stumbleupon circle")
  lazy val IconStumbleupon: Icon        = Icon("stumbleupon")
  lazy val IconTencentWeibo: Icon       = Icon("tencent weibo")
  lazy val IconTrello: Icon             = Icon("trello")
  lazy val IconTumblr: Icon             = Icon("tumblr")
  lazy val IconTumblrSquare: Icon       = Icon("tumblr square")
  lazy val IconTwitch: Icon             = Icon("twitch")
  lazy val IconTwitter: Icon            = Icon("twitter")
  lazy val IconTwitterSquare: Icon      = Icon("twitter square")
  lazy val IconViacoin: Icon            = Icon("viacoin")
  lazy val IconVimeo: Icon              = Icon("vimeo")
  lazy val IconVine: Icon               = Icon("vine")
  lazy val IconVk: Icon                 = Icon("vk")
  lazy val IconWechat: Icon             = Icon("wechat")
  lazy val IconWeibo: Icon              = Icon("weibo")
  lazy val IconWhatsapp: Icon           = Icon("whatsapp")
  lazy val IconWindows: Icon            = Icon("windows")
  lazy val IconWordpress: Icon          = Icon("wordpress")
  lazy val IconXing: Icon               = Icon("xing")
  lazy val IconXingSquare: Icon         = Icon("xing square")
  lazy val IconYahoo: Icon              = Icon("yahoo")
  lazy val IconYelp: Icon               = Icon("yelp")
  lazy val IconYoutube: Icon            = Icon("youtube")
  lazy val IconYoutubePlay: Icon        = Icon("youtube play")
  lazy val IconYoutubeSquare: Icon      = Icon("youtube square")

  // Aliases
  lazy val IconAttention: Icon          = Icon("attention")
  lazy val IconClose: Icon              = Icon("close")

  sealed trait Flipped

  object Flipped {
    case object NotFlipped extends Flipped
    case object Horizontally extends Flipped
    case object Vertically extends Flipped

    implicit val equal: Equal[Flipped] = Equal.equalA
  }

  sealed trait Rotated

  object Rotated {
    case object NotRotated extends Rotated
    case object Clockwise extends Rotated
    case object CounterClockwise extends Rotated

    implicit val equal: Equal[Rotated] = Equal.equalA
  }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(id: String,
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

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(s: String, children: VdomNode*): Icon = Icon(Props(s), children)
}
