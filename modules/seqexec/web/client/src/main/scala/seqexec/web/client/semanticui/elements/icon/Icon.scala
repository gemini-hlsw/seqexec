// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.icon

import cats.Eq
import seqexec.web.client.semanticui.Size
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.extra.Reusability
import cats.implicits._
import web.client.style._

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
           extraStyles: List[GStyle] = Nil,
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
        p.extraStyles.map(geminiStyleToTagMod).toTagMod,
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
    .configure(Reusability.shouldComponentUpdate)
    .build.withKey(p.key).apply(p)(children: _*)
}

object Icon {
  implicit val iconProps: Reusability[Icon.Props] = Reusability.caseClassExcept[Icon.Props]('onClick)
  implicit val reuse: Reusability[Icon] = Reusability.by(_.p)
  // // Web content icons
  // val IconAlarm: Icon             = Icon("alarm")
  // val IconAlarmSlash: Icon        = Icon("alarm slash")
  // val IconAlarmOutline: Icon      = Icon("alarm outline")
  // val IconAlarmSlashOutline: Icon = Icon("alarm slash outline")
  // val IconAt: Icon                = Icon("at")
  val IconBrowser: Icon           = Icon("browser")
  // val IconBug: Icon               = Icon("bug")
  // val IconCalendarOutline: Icon   = Icon("calendar outline")
  // val IconCalendar: Icon          = Icon("calendar")
  // val IconCloud: Icon             = Icon("cloud")
  // val IconCode: Icon              = Icon("code")
  // val IconComment: Icon           = Icon("comment")
  // val IconComments: Icon          = Icon("comments")
  // val IconCommentOutline: Icon    = Icon("comment outline")
  // val IconCommentsOutline: Icon   = Icon("comments outline")
  // val IconCopyright: Icon         = Icon("copyright")
  // val IconDashboard: Icon         = Icon("dashboard")
  val IconDropdown: Icon          = Icon("dropdown")
  // val IconExternalSquare: Icon    = Icon("external square")
  // val IconExternal: Icon          = Icon("external")
  // val IconEyedropper: Icon        = Icon("eyedropper")
  // val IconFeed: Icon              = Icon("feed")
  // val IconFind: Icon              = Icon("find")
  // val IconHeartbeat: Icon         = Icon("heartbeat")
  // val IconHistory: Icon           = Icon("history")
  // val IconHome: Icon              = Icon("home")
  // val IconIdea: Icon              = Icon("idea")
  val IconInbox: Icon             = Icon("inbox")
  // val IconLab: Icon               = Icon("lab")
  // val IconMail: Icon              = Icon("mail")
  // val IconMailOutline: Icon       = Icon("mail outline")
  // val IconMailSquare: Icon        = Icon("mail square")
  // val IconMap: Icon               = Icon("map")
  // val IconOptions: Icon           = Icon("options")
  // val IconPaintBrush: Icon        = Icon("paint brush")
  // val IconPayment: Icon           = Icon("payment")
  // val IconPhone: Icon             = Icon("phone")
  // val IconPhoneSquare: Icon       = Icon("phone square")
  // val IconPrivacy: Icon           = Icon("privacy")
  // val IconProtect: Icon           = Icon("protect")
  // val IconSearch: Icon            = Icon("search")
  // val IconSetting: Icon           = Icon("setting")
  val IconSettings: Icon          = Icon("settings")
  // val IconShop: Icon              = Icon("shop")
  // val IconSidebar: Icon           = Icon("sidebar")
  // val IconSignal: Icon            = Icon("signal")
  // val IconSitemap: Icon           = Icon("sitemap")
  // val IconTag: Icon               = Icon("tag")
  // val IconTags: Icon              = Icon("tags")
  // val IconTasks: Icon             = Icon("tasks")
  // val IconTerminal: Icon          = Icon("terminal")
  // val IconTextTelephone: Icon     = Icon("text telephone")
  // val IconTicket: Icon            = Icon("ticket")
  // val IconTrophy: Icon            = Icon("trophy")
  // val IconWifi: Icon              = Icon("wifi")
  //
  // // User action icons
  // val IconAdjust: Icon                = Icon("adjust")
  // val IconAddUser: Icon               = Icon("add user")
  // val IconAddToCart: Icon             = Icon("add to cart")
  // val IconArchive: Icon               = Icon("archive")
  val IconBan: Icon                   = Icon("ban")
  // val IconBookmark: Icon              = Icon("bookmark")
  // val IconCall: Icon                  = Icon("call")
  // val IconCallSquare: Icon            = Icon("call square")
  // val IconCloudDownload: Icon         = Icon("cloud download")
  // val IconCloudUpload: Icon           = Icon("cloud upload")
  // val IconCompress: Icon              = Icon("compress")
  // val IconConfigure: Icon             = Icon("configure")
  // val IconDownload: Icon              = Icon("download")
  // val IconEdit: Icon                  = Icon("edit")
  // val IconErase: Icon                 = Icon("erase")
  // val IconExchange: Icon              = Icon("exchange")
  // val IconExternalShare: Icon         = Icon("external share")
  // val IconExpand: Icon                = Icon("expand")
  // val IconFilter: Icon                = Icon("filter")
  // val IconFlag: Icon                  = Icon("flag")
  // val IconFlagOutline: Icon           = Icon("flag outline")
  // val IconForwardMail: Icon           = Icon("forward mail")
  // val IconHide: Icon                  = Icon("hide")
  // val IconInCart: Icon                = Icon("in cart")
  val IconLock: Icon                  = Icon("lock")
  // val IconPin: Icon                   = Icon("pin")
  // val IconPrint: Icon                 = Icon("print")
  // val IconRandom: Icon                = Icon("random")
  // val IconRecycle: Icon               = Icon("recycle")
  val IconRefresh: Icon               = Icon("refresh")
  // val IconRemoveBookmark: Icon        = Icon("remove bookmark")
  // val IconRemoveUser: Icon            = Icon("remove user")
  // val IconRepeat: Icon                = Icon("repeat")
  // val IconReplyAll: Icon              = Icon("reply all")
  val IconReply: Icon                 = Icon("reply")
  // val IconRetweet: Icon               = Icon("retweet")
  // val IconSend: Icon                  = Icon("send")
  // val IconSendOutline: Icon           = Icon("send outline")
  // val IconShareAlternate: Icon        = Icon("share alternate")
  // val IconShareAlternateSquare: Icon  = Icon("share alternate square")
  // val IconShare: Icon                 = Icon("share")
  // val IconShareSquare: Icon           = Icon("share square")
  val IconSignIn: Icon                = Icon("sign in")
  val IconSignOut: Icon               = Icon("sign out")
  // val IconTheme: Icon                 = Icon("theme")
  // val IconTranslate: Icon             = Icon("translate")
  // val IconUndo: Icon                  = Icon("undo")
  // val IconUnhide: Icon                = Icon("unhide")
  // val IconUnlockAlternate: Icon       = Icon("unlock alternate")
  // val IconUnlock: Icon                = Icon("unlock")
  val IconUpload: Icon                = Icon("upload")
  // val IconWait: Icon                  = Icon("wait")
  // val IconWizard: Icon                = Icon("wizard")
  // val IconWrite: Icon                 = Icon("write")
  // val IconWriteSquare: Icon           = Icon("write square")
  //
  // // Message Icon
  // val IconAnnouncement: Icon  = Icon("announcement")
  // val IconBirthday: Icon      = Icon("birthday")
  // val IconHelp: Icon          = Icon("help")
  // val IconHelpCircle: Icon    = Icon("help circle")
  // val IconInfo: Icon          = Icon("info")
  // val IconInfoCircle: Icon    = Icon("info circle")
  // val IconWarning: Icon       = Icon("warning")
  // val IconWarningCircle: Icon = Icon("warning circle")
  // val IconWarningSign: Icon   = Icon("warning sign")
  //
  // // User types
  // val IconChild: Icon    = Icon("child")
  // val IconDoctor: Icon   = Icon("doctor")
  // val IconHandicap: Icon = Icon("handicap")
  // val IconSpy: Icon      = Icon("spy")
  // val IconStudent: Icon  = Icon("student")
  val IconUser: Icon     = Icon("user")
  // val IconUsers: Icon    = Icon("users")
  //
  // // Sexuality icons
  // val IconFemale: Icon                = Icon("female")
  // val IconGay: Icon                   = Icon("gay")
  // val IconHeterosexual: Icon          = Icon("heterosexual")
  // val IconIntergender: Icon           = Icon("intergender")
  // val IconLesbian: Icon               = Icon("lesbian")
  // val IconMale: Icon                  = Icon("male")
  // val IconMan: Icon                   = Icon("man")
  // val IconNeuter: Icon                = Icon("neuter")
  // val IconNonBinaryTransgender: Icon  = Icon("non binary transgender")
  // val IconTransgender: Icon           = Icon("transgender")
  // val IconOtherGender: Icon           = Icon("other gender")
  // val IconOtherGenderHorizontal: Icon = Icon("other gender horizontal")
  // val IconOtherGenderVertical: Icon   = Icon("other gender vertical")
  // val IconWoman: Icon                 = Icon("woman")
  //
  // // Layout icons
  // val IconGridLayout: Icon       = Icon("grid layout")
  // val IconListLayout: Icon       = Icon("list layout")
  // val IconBlockLayout: Icon      = Icon("block layout")
  // val IconZoom: Icon             = Icon("zoom")
  // val IconZoomOut: Icon          = Icon("zoom out")
  // val IconResizeVertical: Icon   = Icon("resize vertical")
  // val IconResizeHorizontal: Icon = Icon("resize horizontal")
  // val IconMaximize: Icon         = Icon("maximize")
  // val IconCrop: Icon             = Icon("crop")
  //
  // // Object icons
  // val IconAnchor: Icon           = Icon("anchor")
  // val IconBar: Icon              = Icon("bar")
  // val IconBomb: Icon             = Icon("bomb")
  // val IconBook: Icon             = Icon("book")
  // val IconBullseye: Icon         = Icon("bullseye")
  // val IconCalculator: Icon       = Icon("calculator")
  // val IconCheckeredFlag: Icon    = Icon("checkered flag")
  // val IconCocktail: Icon         = Icon("cocktail")
  // val IconDiamond: Icon          = Icon("diamond")
  // val IconFax: Icon              = Icon("fax")
  // val IconFireExtinguisher: Icon = Icon("fire extinguisher")
  // val IconFire: Icon             = Icon("fire")
  // val IconGift: Icon             = Icon("gift")
  // val IconLeaf: Icon             = Icon("leaf")
  // val IconLegal: Icon            = Icon("legal")
  // val IconLemon: Icon            = Icon("lemon")
  // val IconLifeRing: Icon         = Icon("life ring")
  // val IconLightning: Icon        = Icon("lightning")
  // val IconMagnet: Icon           = Icon("magnet")
  // val IconMoney: Icon            = Icon("money")
  // val IconMoon: Icon             = Icon("moon")
  // val IconPlane: Icon            = Icon("plane")
  // val IconPuzzle: Icon           = Icon("puzzle")
  // val IconRain: Icon             = Icon("rain")
  // val IconRoad: Icon             = Icon("road")
  // val IconRocket: Icon           = Icon("rocket")
  // val IconShipping: Icon         = Icon("shipping")
  // val IconSoccer: Icon           = Icon("soccer")
  // val IconSuitcase: Icon         = Icon("suitcase")
  // val IconSun: Icon              = Icon("sun")
  // val IconTravel: Icon           = Icon("travel")
  // val IconTreatment: Icon        = Icon("treatment")
  // val IconWorld: Icon            = Icon("world")
  //
  // // Shape Icons
  // val IconAsterisk: Icon           = Icon("asterisk")
  // val IconCertificate: Icon        = Icon("certificate")
  // val IconCircle: Icon             = Icon("circle")
  val IconCircleNotched: Icon      = Icon("circle notched")
  // val IconCircleThin: Icon         = Icon("circle thin")
  val IconCrosshairs: Icon         = Icon("crosshairs")
  // val IconCube: Icon               = Icon("cube")
  // val IconCubes: Icon              = Icon("cubes")
  // val IconEllipsisHorizontal: Icon = Icon("ellipsis horizontal")
  // val IconEllipsisVertical: Icon   = Icon("ellipsis vertical")
  // val IconQuoteLeft: Icon          = Icon("quote left")
  // val IconQuoteRight: Icon         = Icon("quote right")
  // val IconSpinner: Icon            = Icon("spinner")
  // val IconSquare: Icon             = Icon("square")
  // val IconSquareOutline: Icon      = Icon("square outline")
  //
  // // selection icons
  // val IconAddCircle: Icon           = Icon("add circle")
  // val IconAddSquare: Icon           = Icon("add square")
  // val IconCheckCircle: Icon         = Icon("check circle")
  // val IconCheckCircleOutline: Icon  = Icon("check circle outline")
  // val IconCheckSquare: Icon         = Icon("check square")
  // val IconCheckmarkBox: Icon        = Icon("checkmark box")
  val IconCheckmark: Icon           = Icon("checkmark")
  val IconMinusCircle: Icon         = Icon("minus circle")
  // val IconMinus: Icon               = Icon("minus")
  // val IconMinusSquare: Icon         = Icon("minus square")
  // val IconMinusSquareOutline: Icon  = Icon("minus square outline")
  // val IconMove: Icon                = Icon("move")
  // val IconPlus: Icon                = Icon("plus")
  val IconPlusSquareOutline: Icon   = Icon("plus square outline")
  // val IconRadio: Icon               = Icon("radio")
  // val IconRemoveCircle: Icon        = Icon("remove circle")
  // val IconRemoveCircleOutline: Icon = Icon("remove circle outline")
  val IconRemove: Icon              = Icon("remove")
  val IconSelectedRadio: Icon       = Icon("selected radio")
  // val IconToggleOff: Icon           = Icon("toggle off")
  // val IconToggleOn: Icon            = Icon("toggle on")
  //
  // // Media icons
  // val IconAreaChart: Icon   = Icon("area chart")
  // val IconBarChart: Icon    = Icon("bar chart")
  // val IconCameraRetro: Icon = Icon("camera retro")
  // val IconNewspaper: Icon   = Icon("newspaper")
  // val IconFilm: Icon        = Icon("film")
  // val IconLineChart: Icon   = Icon("line chart")
  // val IconPhoto: Icon       = Icon("photo")
  // val IconPieChart: Icon    = Icon("pie chart")
  // val IconSound: Icon       = Icon("sound")
  //
  // // pointers icons
  val IconAngleDoubleDown: Icon         = Icon("angle double down")
  // val IconAngleDoubleLeft: Icon         = Icon("angle double left")
  // val IconAngleDoubleRight: Icon        = Icon("angle double right")
  val IconAngleDoubleUp: Icon           = Icon("angle double up")
  // val IconAngleDown: Icon               = Icon("angle down")
  // val IconAngleLeft: Icon               = Icon("angle left")
  // val IconAngleRight: Icon              = Icon("angle right")
  // val IconAngleUp: Icon                 = Icon("angle up")
  // val IconArrowCircleDown: Icon         = Icon("arrow circle down")
  // val IconArrowCircleLeft: Icon         = Icon("arrow circle left")
  // val IconArrowCircleOutlineDown: Icon  = Icon("arrow circle outline down")
  // val IconArrowCircleOutlineLeft: Icon  = Icon("arrow circle outline left")
  // val IconArrowCircleOutlineRight: Icon = Icon("arrow circle outline right")
  // val IconArrowCircleOutlineUp: Icon    = Icon("arrow circle outline up")
  // val IconArrowCircleRight: Icon        = Icon("arrow circle right")
  // val IconArrowCircleUp: Icon           = Icon("arrow circle up")
  // val IconArrowDown: Icon               = Icon("arrow down")
  // val IconArrowLeft: Icon               = Icon("arrow left")
  // val IconArrowRight: Icon              = Icon("arrow right")
  // val IconArrowUp: Icon                 = Icon("arrow up")
  val IconCaretDown: Icon               = Icon("caret down")
  // val IconCaretLeft: Icon               = Icon("caret left")
  val IconCaretRight: Icon              = Icon("caret right")
  // val IconCaretUp: Icon                 = Icon("caret up")
  // val IconChevronCircleDown: Icon       = Icon("chevron circle down")
  // val IconChevronCircleLeft: Icon       = Icon("chevron circle left")
  // val IconChevronCircleRight: Icon      = Icon("chevron circle right")
  // val IconChevronCircleUp: Icon         = Icon("chevron circle up")
  // val IconChevronDown: Icon             = Icon("chevron down")
  val IconChevronLeft: Icon             = Icon("chevron left")
  val IconChevronRight: Icon            = Icon("chevron right")
  // val IconChevronUp: Icon               = Icon("chevron up")
  // val IconLongArrowDown: Icon           = Icon("long arrow down")
  // val IconLongArrowLeft: Icon           = Icon("long arrow left")
  // val IconLongArrowRight: Icon          = Icon("long arrow right")
  // val IconLongArrowUp: Icon             = Icon("long arrow up")
  // val IconPointingDown: Icon            = Icon("pointing down")
  // val IconPointingLeft: Icon            = Icon("pointing left")
  // val IconPointingRight: Icon           = Icon("pointing right")
  // val IconPointingUp: Icon              = Icon("pointing up")
  // val IconToggleDown: Icon              = Icon("toggle down")
  // val IconToggleLeft: Icon              = Icon("toggle left")
  // val IconToggleRight: Icon             = Icon("toggle right")
  // val IconToggleUp: Icon                = Icon("toggle up")
  //
  // // computer icons
  // val IconDesktop: Icon               = Icon("desktop")
  // val IconDiskOutline: Icon           = Icon("disk outline")
  // val IconFileArchiveOutline: Icon    = Icon("file archive outline")
  // val IconFileAudioOutline: Icon      = Icon("file audio outline")
  // val IconFileCodeOutline: Icon       = Icon("file code outline")
  // val IconFileExcelOutline: Icon      = Icon("file excel outline")
  // val IconFile: Icon                  = Icon("file")
  // val IconFileImageOutline: Icon      = Icon("file image outline")
  // val IconFileOutline: Icon           = Icon("file outline")
  // val IconFilePdfOutline: Icon        = Icon("file pdf outline")
  // val IconFilePowerpointOutline: Icon = Icon("file powerpoint outline")
  // val IconFileText: Icon              = Icon("file text")
  // val IconFileTextOutline: Icon       = Icon("file text outline")
  // val IconFileVideoOutline: Icon      = Icon("file video outline")
  // val IconFileWordOutline: Icon       = Icon("file word outline")
  // val IconFolder: Icon                = Icon("folder")
  // val IconFolderOpen: Icon            = Icon("folder open")
  // val IconFolderOpenOutline: Icon     = Icon("folder open outline")
  // val IconFolderOutline: Icon         = Icon("folder outline")
  // val IconGame: Icon                  = Icon("game")
  // val IconKeyboard: Icon              = Icon("keyboard")
  // val IconLaptop: Icon                = Icon("laptop")
  // val IconLevelDown: Icon             = Icon("level down")
  // val IconLevelUp: Icon               = Icon("level up")
  // val IconMobile: Icon                = Icon("mobile")
  // val IconPower: Icon                 = Icon("power")
  // val IconPlug: Icon                  = Icon("plug")
  // val IconTablet: Icon                = Icon("tablet")
  val IconTrash: Icon                 = Icon("trash")
  // val IconTrashOutline: Icon          = Icon("trash outline")
  //
  // // technologies icon
  // val IconBarcode: Icon   = Icon("barcode")
  // val IconCss3: Icon      = Icon("css3")
  // val IconDatabase: Icon  = Icon("database")
  // val IconFork: Icon      = Icon("fork")
  // val IconHtml5: Icon     = Icon("html5")
  // val IconOpenid: Icon    = Icon("openid")
  // val IconQrcode: Icon    = Icon("qrcode")
  // val IconRSS: Icon       = Icon("rss")
  // val IconRSSSquare: Icon = Icon("rss square")
  // val IconServer: Icon    = Icon("server")
  //
  // // rating icons
  // val IconEmptyHeart: Icon        = Icon("empty heart")
  // val IconEmptyStar: Icon         = Icon("empty star")
  // val IconFrown: Icon             = Icon("frown")
  // val IconHeart: Icon             = Icon("heart")
  // val IconMeh: Icon               = Icon("meh")
  // val IconSmile: Icon             = Icon("smile")
  // val IconStarHalfEmpty: Icon     = Icon("star half empty")
  // val IconStarHalf: Icon          = Icon("star half")
  // val IconStar: Icon              = Icon("star")
  // val IconThumbsDown: Icon        = Icon("thumbs down")
  // val IconThumbsOutlineDown: Icon = Icon("thumbs outline down")
  // val IconThumbsOutlineUp: Icon   = Icon("thumbs outline up")
  // val IconThumbsUp: Icon          = Icon("thumbs up")
  //
  // // audio icons
  // val IconBackward: Icon          = Icon("backward")
  // val IconEject: Icon             = Icon("eject")
  // val IconFastBackward: Icon      = Icon("fast backward")
  // val IconFastForward: Icon       = Icon("fast forward")
  // val IconForward: Icon           = Icon("forward")
  // val IconMusic: Icon             = Icon("music")
  // val IconMute: Icon              = Icon("mute")
  val IconPause: Icon             = Icon("pause")
  val IconPlay: Icon              = Icon("play")
  // val IconRecord: Icon            = Icon("record")
  // val IconStepBackward: Icon      = Icon("step backward")
  // val IconStepForward: Icon       = Icon("step forward")
  val IconStop: Icon              = Icon("stop")
  val IconStopCircle: Icon        = Icon("stop circle")
  // val IconStopCircleOutline: Icon = Icon("stop circle outline")
  // val IconUnmute: Icon            = Icon("unmute")
  // val IconVideoPlay: Icon         = Icon("video play")
  // val IconVideoPlayOutline: Icon  = Icon("video play outline")
  // val IconVolumeDown: Icon        = Icon("volume down")
  // val IconVolumeOff: Icon         = Icon("volume off")
  // val IconVolumeUp: Icon          = Icon("volume up")
  //
  // // map icons
  // val IconBuilding: Icon        = Icon("building")
  // val IconBuildingOutline: Icon = Icon("building outline")
  // val IconCar: Icon             = Icon("car")
  // val IconCoffee: Icon          = Icon("coffee")
  // val IconEmergency: Icon       = Icon("emergency")
  // val IconFirstAid: Icon        = Icon("first aid")
  // val IconFood: Icon            = Icon("food")
  // val IconH: Icon               = Icon("h")
  // val IconHospital: Icon        = Icon("hospital")
  // val IconLocationArrow: Icon   = Icon("location arrow")
  // val IconMarker: Icon          = Icon("marker")
  // val IconMilitary: Icon        = Icon("military")
  // val IconPaw: Icon             = Icon("paw")
  // val IconSpaceShuttle: Icon    = Icon("space shuttle")
  // val IconSpoon: Icon           = Icon("spoon")
  // val IconTaxi: Icon            = Icon("taxi")
  // val IconTree: Icon            = Icon("tree")
  // val IconUniversity: Icon      = Icon("university")
  //
  // // tables
  // val IconColumns: Icon                = Icon("columns")
  // val IconSortAlphabetAscending: Icon  = Icon("sort alphabet ascending")
  // val IconSortAlphabetDescending: Icon = Icon("sort alphabet descending")
  // val IconSortAscending: Icon          = Icon("sort ascending")
  // val IconSortContentAscending: Icon   = Icon("sort content ascending")
  // val IconSortContentDescending: Icon  = Icon("sort content descending")
  // val IconSortDescending: Icon         = Icon("sort descending")
  // val IconSort: Icon                   = Icon("sort")
  // val IconSortNumericAscending: Icon   = Icon("sort numeric ascending")
  // val IconSortNumericDescending: Icon  = Icon("sort numeric descending")
  // val IconTable: Icon                  = Icon("table")
  //
  // // text editor icons
  // val IconAlignCenter: Icon   = Icon("align center")
  // val IconAlignJustify: Icon  = Icon("align justify")
  // val IconAlignLeft: Icon     = Icon("align left")
  // val IconAlignRight: Icon    = Icon("align right")
  // val IconAttach: Icon        = Icon("attach")
  // val IconBold: Icon          = Icon("bold")
  val IconCopy: Icon          = Icon("copy")
  // val IconCut: Icon           = Icon("cut")
  // val IconFont: Icon          = Icon("font")
  // val IconHeader: Icon        = Icon("header")
  // val IconIndent: Icon        = Icon("indent")
  // val IconItalic: Icon        = Icon("italic")
  // val IconLinkify: Icon       = Icon("linkify")
  // val IconList: Icon          = Icon("list")
  // val IconOrderedList: Icon   = Icon("ordered list")
  // val IconOutdent: Icon       = Icon("outdent")
  // val IconParagraph: Icon     = Icon("paragraph")
  // val IconPaste: Icon         = Icon("paste")
  // val IconSave: Icon          = Icon("save")
  // val IconStrikethrough: Icon = Icon("strikethrough")
  // val IconSubscript: Icon     = Icon("subscript")
  // val IconSuperscript: Icon   = Icon("superscript")
  // val IconTextHeight: Icon    = Icon("text height")
  // val IconTextWidth: Icon     = Icon("text width")
  // val IconUnderline: Icon     = Icon("underline")
  // val IconUnlink: Icon        = Icon("unlink")
  // val IconUnorderedList: Icon = Icon("unordered list")
  //
  // // Currency icons
  // val IconDollar: Icon = Icon("dollar")
  // val IconEuro: Icon   = Icon("euro")
  // val IconLira: Icon   = Icon("lira")
  // val IconPound: Icon  = Icon("pound")
  // val IconRuble: Icon  = Icon("ruble")
  // val IconRupee: Icon  = Icon("rupee")
  // val IconShekel: Icon = Icon("shekel")
  // val IconWon: Icon    = Icon("won")
  // val IconYen: Icon    = Icon("yen")
  //
  // // payment icons
  // val IconAmericanExpress: Icon = Icon("american express")
  // val IconDiscover: Icon        = Icon("discover")
  // val IconGoogleWallet: Icon    = Icon("google wallet")
  // val IconMastercard: Icon      = Icon("mastercard")
  // val IconPaypalCard: Icon      = Icon("paypal card")
  // val IconPaypal: Icon          = Icon("paypal")
  // val IconStripe: Icon          = Icon("stripe")
  // val IconVisa: Icon            = Icon("visa")
  //
  // // Brands
  // val IconAdn: Icon                = Icon("adn")
  // val IconAndroid: Icon            = Icon("android")
  // val IconAngellist: Icon          = Icon("angellist")
  // val IconApple: Icon              = Icon("apple")
  // val IconBehance: Icon            = Icon("behance")
  // val IconBehanceSquare: Icon      = Icon("behance square")
  // val IconBitbucket: Icon          = Icon("bitbucket")
  // val IconBitbucketSquare: Icon    = Icon("bitbucket square")
  // val IconBitcoin: Icon            = Icon("bitcoin")
  // val IconBuysellads: Icon         = Icon("buysellads")
  // val IconCodepen: Icon            = Icon("codepen")
  // val IconConnectdevelop: Icon     = Icon("connectdevelop")
  // val IconDashcube: Icon           = Icon("dashcube")
  // val IconDelicious: Icon          = Icon("delicious")
  // val IconDeviantart: Icon         = Icon("deviantart")
  // val IconDigg: Icon               = Icon("digg")
  // val IconDribbble: Icon           = Icon("dribbble")
  // val IconDropbox: Icon            = Icon("dropbox")
  // val IconDrupal: Icon             = Icon("drupal")
  // val IconEmpire: Icon             = Icon("empire")
  // val IconFacebook: Icon           = Icon("facebook")
  // val IconFacebookSquare: Icon     = Icon("facebook square")
  // val IconFlickr: Icon             = Icon("flickr")
  // val IconForumbee: Icon           = Icon("forumbee")
  // val IconFoursquare: Icon         = Icon("foursquare")
  // val IconGit: Icon                = Icon("git")
  // val IconGitSquare: Icon          = Icon("git square")
  // val IconGithubAlternate: Icon    = Icon("github alternate")
  // val IconGithub: Icon             = Icon("github")
  // val IconGithubSquare: Icon       = Icon("github square")
  // val IconGittip: Icon             = Icon("gittip")
  // val IconGoogle: Icon             = Icon("google")
  // val IconGooglePlus: Icon         = Icon("google plus")
  // val IconGooglePlusSquare: Icon   = Icon("google plus square")
  // val IconHackerNews: Icon         = Icon("hacker news")
  // val IconInstagram: Icon          = Icon("instagram")
  // val IconIoxhost: Icon            = Icon("ioxhost")
  // val IconJoomla: Icon             = Icon("joomla")
  // val IconJsfiddle: Icon           = Icon("jsfiddle")
  // val IconLastfm: Icon             = Icon("lastfm")
  // val IconLastfmSquare: Icon       = Icon("lastfm square")
  // val IconLeanpub: Icon            = Icon("leanpub")
  // val IconLinkedin: Icon           = Icon("linkedin")
  // val IconLinkedinSquare: Icon     = Icon("linkedin square")
  // val IconLinux: Icon              = Icon("linux")
  // val IconMaxcdn: Icon             = Icon("maxcdn")
  // val IconMeanpath: Icon           = Icon("meanpath")
  // val IconMedium: Icon             = Icon("medium")
  // val IconPagelines: Icon          = Icon("pagelines")
  // val IconPiedPiperAlternate: Icon = Icon("pied piper alternate")
  // val IconPiedPiper: Icon          = Icon("pied piper")
  // val IconPinterest: Icon          = Icon("pinterest")
  // val IconPinterestSquare: Icon    = Icon("pinterest square")
  // val IconQq: Icon                 = Icon("qq")
  // val IconRebel: Icon              = Icon("rebel")
  // val IconReddit: Icon             = Icon("reddit")
  // val IconRedditSquare: Icon       = Icon("reddit square")
  // val IconRenren: Icon             = Icon("renren")
  // val IconSellsy: Icon             = Icon("sellsy")
  // val IconShirtsinbulk: Icon       = Icon("shirtsinbulk")
  // val IconSimplybuilt: Icon        = Icon("simplybuilt")
  // val IconSkyatlas: Icon           = Icon("skyatlas")
  // val IconSkype: Icon              = Icon("skype")
  // val IconSlack: Icon              = Icon("slack")
  // val IconSlideshare: Icon         = Icon("slideshare")
  // val IconSoundcloud: Icon         = Icon("soundcloud")
  // val IconSpotify: Icon            = Icon("spotify")
  // val IconStackExchange: Icon      = Icon("stack exchange")
  // val IconStackOverflow: Icon      = Icon("stack overflow")
  // val IconSteam: Icon              = Icon("steam")
  // val IconSteamSquare: Icon        = Icon("steam square")
  // val IconStumbleuponCircle: Icon  = Icon("stumbleupon circle")
  // val IconStumbleupon: Icon        = Icon("stumbleupon")
  // val IconTencentWeibo: Icon       = Icon("tencent weibo")
  // val IconTrello: Icon             = Icon("trello")
  // val IconTumblr: Icon             = Icon("tumblr")
  // val IconTumblrSquare: Icon       = Icon("tumblr square")
  // val IconTwitch: Icon             = Icon("twitch")
  // val IconTwitter: Icon            = Icon("twitter")
  // val IconTwitterSquare: Icon      = Icon("twitter square")
  // val IconViacoin: Icon            = Icon("viacoin")
  // val IconVimeo: Icon              = Icon("vimeo")
  // val IconVine: Icon               = Icon("vine")
  // val IconVk: Icon                 = Icon("vk")
  // val IconWechat: Icon             = Icon("wechat")
  // val IconWeibo: Icon              = Icon("weibo")
  // val IconWhatsapp: Icon           = Icon("whatsapp")
  // val IconWindows: Icon            = Icon("windows")
  // val IconWordpress: Icon          = Icon("wordpress")
  // val IconXing: Icon               = Icon("xing")
  // val IconXingSquare: Icon         = Icon("xing square")
  // val IconYahoo: Icon              = Icon("yahoo")
  // val IconYelp: Icon               = Icon("yelp")
  // val IconYoutube: Icon            = Icon("youtube")
  // val IconYoutubePlay: Icon        = Icon("youtube play")
  // val IconYoutubeSquare: Icon      = Icon("youtube square")
  //
  // // Aliases
  val IconAttention: Icon          = Icon("attention")
  val IconClose: Icon              = Icon("close")

  sealed trait Flipped

  object Flipped {
    case object NotFlipped extends Flipped
    case object Horizontally extends Flipped
    case object Vertically extends Flipped

    implicit val equal: Eq[Flipped] = Eq.fromUniversalEquals
    implicit val reuse: Reusability[Flipped] = Reusability.byRef[Flipped]
  }

  sealed trait Rotated

  object Rotated {
    case object NotRotated extends Rotated
    case object Clockwise extends Rotated
    case object CounterClockwise extends Rotated

    implicit val equal: Eq[Rotated] = Eq.fromUniversalEquals
    implicit val reuse: Reusability[Rotated] = Reusability.byRef[Rotated]
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
                   extraStyles: List[GStyle] = Nil,
                   key: String = "",
                   onClick: Callback = Callback.empty)

  // Used to call Icon directly on a jsx component declaration
  implicit def icon2TagMod(i: Icon): VdomElement = i.component

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(s: String, children: VdomNode*): Icon = Icon(Props(s), children)
}
