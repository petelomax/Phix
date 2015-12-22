--
-- Copyright (C) 2008-2010 by Jeremy Cowgar <jeremy@cowgar.com>
--
-- This file is part of EuIup.
--
-- EuIup is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- EuIup is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with EuIup.  If not, see <http://www.gnu.org/licenses/>.
--

namespace iup

global function iup_isprint(integer c)
    return c>31 and c<127
end function
global constant IUP_MASK_UINT = "/d+"

--****
-- == Iup Constants
--

--****
-- === Common return values
--

global constant
        ERROR = 1,
        NOERROR = 0,
        OPENED = -1,
        INVALID = -1

--****
-- === Callback return values
--

global constant
        IUP_IGNORE = -1,
        IUP_DEFAULT = -2,
        IUP_CLOSE = -3,
        IUP_CONTINUE = -4

--****
-- === Mouse button values
--

global constant
        BUTTON1 = '1',
        BUTTON2 = '2',
        BUTTON3 = '3',
        BUTTON4 = '4',
        BUTTON5 = '5'

--****
-- === Smart positioning values
--

global constant
        IUP_CENTER = 65535,
        LEFT = 65534,
        RIGHT = 65533,
        MOUSEPOS = 65532,
        CURRENT = 66531,
        IUP_CENTERPARENT = 65530,
--public constant IUP_CENTERPARENT = 0xFFFA /* 65530 */
        TOP = IUP_CENTER,
        BOTTOM = RIGHT,
        ANYWHERE = CURRENT

--****
-- === Generic
--

global constant
        RUN = "RUN",
        ENGLISH = "ENGLISH",
        PORTUGUESE = "PORTUGUESE",
        SBH = "SBH",
        SBV = "SBV",
        --
        -- Acoes
        --
        DEFAULT_ACTION = "DEFAULT_ACTION",
        IDLE_ACTION = "IDLE_ACTION",
        ACTION = "ACTION",
        ACTION_CB = "ACTION_CB",
        GETFOCUS_CB = "GETFOCUS_CB",
        KILLFOCUS_CB = "KILLFOCUS_CB",
        K_ANY = "K_ANY",
        KEYPRESS_CB = "KEYPRESS_CB",
        HELP_CB = "HELP_CB",
        SCROLL_CB = "SCROLL_CB",
        RESIZE_CB = "RESIZE_CB",
        MOTION_CB = "MOTION_CB",
        BUTTON_CB = "BUTTON_CB",
        ENTERWINDOW_CB = "ENTERWINDOW_CB",
        LEAVEWINDOW_CB = "LEAVEWINDOW_CB",
        WHEEL_CB = "WHEEL_CB",
        MASK_CB = "MASK_CB",
        OPEN_CB = "OPEN_CB",
        HIGHLIGHT_CB = "HIGHLIGHT_CB",
        MENUCLOSE_CB = "MENUCLOSE_CB",
        MAP_CB = "MAP_CB",
        CLOSE_CB = "CLOSE_CB",
        SHOW_CB = "SHOW_CB",
        DROPFILES_CB = "DROPFILES_CB",
        WOM_CB = "WOM_CB",
        --
        -- Atributos
        --
        DIRECTION = "DIRECTION",
        ACTIVE = "ACTIVE",
        BGCOLOR = "BGCOLOR",
        FRAMECOLOR = "FRAMECOLOR",
        FGCOLOR = "FGCOLOR",
        COLOR = "COLOR",
        WID = "WID",
        SIZE = "SIZE",
        RASTERSIZE = "RASTERSIZE",
        TITLE = "TITLE",
        IUP_VALUE = "VALUE",
        VISIBLE = "VISIBLE",
        FONT = "FONT",
        TIP = "TIP",
        EXPAND = "EXPAND",
        SEPARATOR = "SEPARATOR",
        HOTSPOT = "HOTSPOT",
        HEIGHT = "HEIGHT",
        WIDTH = "WIDTH",
        KEY = "KEY",
        MULTIPLE = "MULTIPLE",
        DROPDOWN = "DROPDOWN",
        VISIBLE_ITEMS = "VISIBLE_ITEMS",
        MARGIN = "MARGIN",
        GAP = "GAP",
        ALIGNMENT = "ALIGNMENT",
        IMAGE = "IMAGE",
        IMINACTIVE = "IMINACTIVE",
        IMPRESS = "IMPRESS",
        WIN_SAVEBITS = "WIN_SAVEBITS",
        NC = "NC",
        MASK = "MASK",
        APPEND = "APPEND",
        BORDER = "BORDER",
        CARET = "CARET",
        SELECTION = "SELECTION",
        SELECTEDTEXT = "SELECTEDTEXT",
        INSERT = "INSERT",
        CONID = "CONID",
        CURSOR = "CURSOR",
        ICON = "ICON",
        MENUBOX = "MENUBOX",
        MINBOX = "MINBOX",
        MAXBOX = "MAXBOX",
        RESIZE = "RESIZE",
        IUP_MENU = "MENU",
        STARTFOCUS = "STARTFOCUS",
        PARENTDIALOG = "PARENTDIALOG",
        SHRINK = "SHRINK",
        DEFAULTENTER = "DEFAULTENTER",
        DEFAULTESC = "DEFAULTESC",
        X = "X",
        Y = "Y",
        TOOLBOX = "TOOLBOX",
        CONTROL = "CONTROL",
        READONLY = "READONLY",
        SCROLLBAR = "SCROLLBAR",
        POSY = "POSY",
        POSX = "POSX",
        DX = "DX",
        DY = "DY",
        XMAX = "XMAX",
        XMIN = "XMIN",
        YMAX = "YMAX",
        YMIN = "YMIN",
        RED = "255 0 0",
        GREEN = "0 255 0",
        BLUE = "0 0 255",
        MIN = "MIN",
        MAX = "MAX",
        TIME = "TIME",
        DRAG = "DRAG",
        DROP = "DROP",
        REPAINT = "REPAINT",
        TOPMOST = "TOPMOST",
        CLIPCHILDREN = "CLIPCHILDREN",
        DIALOGTYPE = "DIALOGTYPE",
        FILE = "FILE",
        MULTIPLEFILES = "MULTIPLEFILES",
        FILTER = "FILTER",
        FILTERUSED = "FILTERUSED",
        FILTERINFO = "FILTERINFO",
        EXTFILTER = "EXTFILTER",
        DIRECTORY = "DIRECTORY",
        ALLOWNEW = "ALLOWNEW",
        NOOVERWRITEPROMPT = "NOOVERWRITEPROMPT",
        NOCHANGEDIR = "NOCHANGEDIR",
        FILEEXIST = "FILEEXIST",
        STATUS = "STATUS",
        LOCKLOOP = "LOCKLOOP",
        SYSTEM = "SYSTEM",
        DRIVER = "DRIVER",
        SCREENSIZE = "SCREENSIZE",
        SYSTEMLANGUAGE = "SYSTEMLANGUAGE",
        COMPUTERNAME = "COMPUTERNAME",
        USERNAME = "USERNAME",
        --
        --                   Valores para o filedlg
        --
        OPEN = "OPEN",
        SAVE = "SAVE",
        DIR = "DIR",
        --
        -- Valores para scrollbar
        --
        HORIZONTAL = "HORIZONTAL",
        VERTICAL = "VERTICAL",
        --
        -- Valores dos Atributos
        --
        YES = "YES",
        NO = "NO",
        ON = "ON",
        OFF = "OFF",
        ACENTER = "ACENTER",
        ALEFT = "ALEFT",
        ARIGHT = "ARIGHT",
        ATOP = "ATOP",
        ABOTTOM = "ABOTTOM",
        NORTH = "NORTH",
        SOUTH = "SOUTH",
        WEST = "WEST",
        EAST = "EAST",
        NE = "NE",
        SE = "SE",
        NW = "NW",
        SW = "SW",
        FULLSCREEN = "FULLSCREEN",
        FULL = "FULL",
        HALF = "HALF",
        THIRD = "THIRD",
        QUARTER = "QUARTER",
        EIGHTH = "EIGHTH",
        ARROW = "ARROW",
        BUSY = "BUSY",
        RESIZE_N = "RESIZE_N",
        RESIZE_S = "RESIZE_S",
        RESIZE_E = "RESIZE_E",
        RESIZE_W = "RESIZE_W",
        RESIZE_NE = "RESIZE_NE",
        RESIZE_NW = "RESIZE_NW",
        RESIZE_SE = "RESIZE_SE",
        RESIZE_SW = "RESIZE_SW",
        MOVE = "MOVE",
        HAND = "HAND",
        NONE = "NONE",
        IUP = "IUP",
        CROSS = "CROSS",
        PEN = "PEN",
        TEXT = "TEXT",
        RESIZE_C = "RESIZE_C",
        OPENHAND = "OPENHAND",
        HELVETICA_NORMAL_8 = "HELVETICA_NORMAL_8",
        HELVETICA_ITALIC_8 = "HELVETICA_ITALIC_8",
        HELVETICA_BOLD_8 = "HELVETICA_BOLD_8",
        HELVETICA_NORMAL_10 = "HELVETICA_NORMAL_10",
        HELVETICA_ITALIC_10 = "HELVETICA_ITALIC_10",
        HELVETICA_BOLD_10 = "HELVETICA_BOLD_10",
        HELVETICA_NORMAL_12 = "HELVETICA_NORMAL_12",
        HELVETICA_ITALIC_12 = "HELVETICA_ITALIC_12",
        HELVETICA_BOLD_12 = "HELVETICA_BOLD_12",
        HELVETICA_NORMAL_14 = "HELVETICA_NORMAL_14",
        HELVETICA_ITALIC_14 = "HELVETICA_ITALIC_14",
        HELVETICA_BOLD_14 = "HELVETICA_BOLD_14",
        COURIER_NORMAL_8 = "COURIER_NORMAL_8",
        COURIER_ITALIC_8 = "COURIER_ITALIC_8",
        COURIER_BOLD_8 = "COURIER_BOLD_8",
        COURIER_NORMAL_10 = "COURIER_NORMAL_10",
        COURIER_ITALIC_10 = "COURIER_ITALIC_10",
        COURIER_BOLD_10 = "COURIER_BOLD_10",
        COURIER_NORMAL_12 = "COURIER_NORMAL_12",
        COURIER_ITALIC_12 = "COURIER_ITALIC_12",
        COURIER_BOLD_12 = "COURIER_BOLD_12",
        COURIER_NORMAL_14 = "COURIER_NORMAL_14",
        COURIER_ITALIC_14 = "COURIER_ITALIC_14",
        COURIER_BOLD_14 = "COURIER_BOLD_14",
        TIMES_NORMAL_8 = "TIMES_NORMAL_8",
        TIMES_ITALIC_8 = "TIMES_ITALIC_8",
        TIMES_BOLD_8 = "TIMES_BOLD_8",
        TIMES_NORMAL_10 = "TIMES_NORMAL_10",
        TIMES_ITALIC_10 = "TIMES_ITALIC_10",
        TIMES_BOLD_10 = "TIMES_BOLD_10",
        TIMES_NORMAL_12 = "TIMES_NORMAL_12",
        TIMES_ITALIC_12 = "TIMES_ITALIC_12",
        TIMES_BOLD_12 = "TIMES_BOLD_12",
        TIMES_NORMAL_14 = "TIMES_NORMAL_14",
        TIMES_ITALIC_14 = "TIMES_ITALIC_14",
        TIMES_BOLD_14 = "TIMES_BOLD_14",
        --
        -- Dial
        --
        ICTL_DENSITY = "DENSITY",
        ICTL_HORIZONTAL = "HORIZONTAL",
        ICTL_VERTICAL = "VERTICAL",
        ICTL_CIRCULAR = "CIRCULAR",
        ICTL_UNIT = "UNIT",
        --
        -- Matrix
        --
        ENTERITEM_CB = "ENTERITEM_CB",
        LEAVEITEM_CB = "LEAVEITEM_CB",
        EDITION_CB = "EDITION_CB",
        CLICK_CB = "CLICK_CB",
        DROP_CB = "DROP_CB",
        DROPSELECT_CB = "DROPSELECT_CB",
        DROPCHECK_CB = "DROPCHECK_CB",
        VALUE_CB = "VALUE_CB",
        VALUE_EDIT_CB = "VALUE_EDIT_CB",
        FIELD_CB = "FIELD_CB",
        RESIZEMATRIX = "RESIZEMATRIX",
        ADDLIN = "ADDLIN",
        ADDCOL = "ADDCOL",
        DELLIN = "DELLIN",
        DELCOL = "DELCOL",
        NUMLIN = "NUMLIN",
        NUMCOL = "NUMCOL",
        NUMLIN_VISIBLE = "NUMLIN_VISIBLE",
        NUMCOL_VISIBLE = "NUMCOL_VISIBLE",
        MARKED = "MARKED",
        WIDTHDEF = "WIDTHDEF",
        HEIGHTDEF = "HEIGHTDEF",
        AREA = "AREA",
        MARK_MODE = "MARK_MODE",
        LIN = "LIN",
        COL = "COL",
        LINCOL = "LINCOL",
        CELL = "CELL",
        EDIT_MODE = "EDIT_MODE",
        FOCUS_CELL = "FOCUS_CELL",
        ORIGIN = "ORIGIN",
        REDRAW = "REDRAW",
        PREVIOUSVALUE = "PREVIOUSVALUE",
        MOUSEMOVE_CB = "MOUSEMOVE_CB",
        --
        -- Tree
        --
        ADDLEAF = "ADDLEAF",
        ADDBRANCH = "ADDBRANCH",
        DELNODE = "DELNODE",
        IMAGELEAF = "IMAGELEAF",
        IMAGEBRANCHCOLLAPSED = "IMAGEBRANCHCOLLAPSED",
        IMAGEBRANCHEXPANDED = "IMAGEBRANCHEXPANDED",
        IMAGEEXPANDED = "IMAGEEXPANDED",
        KIND = "KIND",
        PARENT = "PARENT",
        DEPTH = "DEPTH",
        ADDEXPANDED = "ADDEXPANDED",
        CTRL = "CTRL",
        SHIFT = "SHIFT",
        STATE = "STATE",
        STARTING = "STARTING",
        LEAF = "LEAF",
        BRANCH = "BRANCH",
        SELECTED = "SELECTED",
        CHILDREN = "CHILDREN",
        ROOT = "ROOT",
        LAST = "LAST",
        PGUP = "PGUP",
        PGDN = "PGDN",
        NEXT = "NEXT",
        PREVIOUS = "PREVIOUS",
        INVERT = "INVERT",
        BLOCK = "BLOCK",
        CLEARALL = "CLEARALL",
        MARKALL = "MARKALL",
        INVERTALL = "INVERTALL",
        COLLAPSED = "COLLAPSED",
        EXPANDED = "EXPANDED",
        SELECTION_CB = "SELECTION_CB",
        BRANCHOPEN_CB = "BRANCHOPEN_CB",
        BRANCHCLOSE_CB = "BRANCHCLOSE_CB",
        RIGHTCLICK_CB = "RIGHTCLICK_CB",
        EXECUTELEAF_CB = "EXECUTELEAF_CB",
        RENAMENODE_CB = "RENAMENODE_CB",
        IMGLEAF = "IMGLEAF",
        IMGCOLLAPSED = "IMGCOLLAPSED",
        IMGEXPANDED = "IMGEXPANDED",
        IMGBLANK = "IMGBLANK",
        IMGPAPER = "IMGPAPER",
        FORMATTING = "FORMATTING",
        SCROLLTO = "SCROLLTO",
        SCROLLTOPOS = "SCROLLTOPOS",
        CARETPOS = "CARETPOS"

global enum
        IUP_RECBINARY = 0,
        IUP_RECTEXT,
$

--****
-- === Keyboard
--

--/*
global constant
        K_exclam = "K_exclam",
        K_quotedbl = "K_quotedbl",
        K_numbersign = "K_numbersign",
        K_dollar = "K_dollar",
        K_percent = "K_percent",
        K_ampersand = "K_ampersand",
        K_quoteright = "K_quoteright",
        K_parentleft = "K_parentleft",
        K_parentright = "K_parentright",
        K_asterisk = "K_asterisk",
        K_plus = "K_plus",
        K_comma = "K_comma",
        K_minus = "K_minus",
        K_period = "K_period",
        K_slash = "K_slash",
        K_0 = "K_0",
        K_1 = "K_1",
        K_2 = "K_2",
        K_3 = "K_3",
        K_4 = "K_4",
        K_5 = "K_5",
        K_6 = "K_6",
        K_7 = "K_7",
        K_8 = "K_8",
        K_9 = "K_9",
        K_colon = "K_colon",
        K_semicolon = "K_semicolon = ",
        K_less = "K_less",
        K_equal = "K_equal",
        K_greater = "K_greater",
        K_question = "K_question",
        K_at = "K_at",
        K_A = "K_A",
        K_B = "K_B",
        K_C = "K_C",
        K_D = "K_D",
        K_E = "K_E",
        K_F = "K_F",
        K_G = "K_G",
        K_H = "K_H",
        K_I = "K_I",
        K_J = "K_J",
        K_K = "K_K",
        K_L = "K_L",
        K_M = "K_M",
        K_N = "K_N",
        K_O = "K_O",
        K_P = "K_P",
        K_Q = "K_Q",
        K_R = "K_R",
        K_S = "K_S",
        K_T = "K_T",
        K_U = "K_U",
        K_V = "K_V",
        K_W = "K_W",
        K_X = "K_X",
        K_Y = "K_Y",
        K_Z = "K_Z",
        K_bracketleft = "K_bracketleft",
        K_backslash = "K_backslash",
        K_bracketright = "K_bracketright",
        K_circum = "K_circum",
        K_underscore = "K_underscore",
        K_quoteleft = "K_quoteleft",
        K_a = "K_a",
        K_b = "K_b",
        K_c = "K_c",
        K_d = "K_d",
        K_e = "K_e",
        K_f = "K_f",
        K_g = "K_g",
        K_h = "K_h",
        K_i = "K_i",
        K_j = "K_j",
        K_k = "K_k",
        K_l = "K_l",
        K_m = "K_m",
        K_n = "K_n",
        K_o = "K_o",
        K_p = "K_p",
        K_q = "K_q",
        K_r = "K_r",
        K_s = "K_s",
        K_t = "K_t",
        K_u = "K_u",
        K_v = "K_v",
        K_w = "K_w",
        K_x = "K_x",
        K_y = "K_y",
        K_z = "K_z",
        K_braceleft = "K_braceleft",
        K_bar = "K_bar",
        K_braceright = "K_braceright",
        K_tilde = "K_tilde",
        K_cA = "K_cA",
        K_cB = "K_cB",
        K_cC = "K_cC",
        K_cD = "K_cD",
        K_cE = "K_cE",
        K_cF = "K_cF",
        K_cG = "K_cG",
        K_cJ = "K_cJ",
        K_cK = "K_cK",
        K_cL = "K_cL",
        K_cN = "K_cN",
        K_cO = "K_cO",
        K_cP = "K_cP",
        K_cQ = "K_cQ",
        K_cR = "K_cR",
        K_cS = "K_cS",
        K_cT = "K_cT",
        K_cU = "K_cU",
        K_cV = "K_cV",
        K_cW = "K_cW",
        K_cX = "K_cX",
        K_cY = "K_cY",
        K_cZ = "K_cZ",
        K_mA = "K_mA",
        K_mB = "K_mB",
        K_mC = "K_mC",
        K_mD = "K_mD",
        K_mE = "K_mE",
        K_mF = "K_mF",
        K_mG = "K_mG",
        K_mH = "K_mH",
        K_mI = "K_mI",
        K_mJ = "K_mJ",
        K_mK = "K_mK",
        K_mL = "K_mL",
        K_mM = "K_mM",
        K_mN = "K_mN",
        K_mO = "K_mO",
        K_mP = "K_mP",
        K_mQ = "K_mQ",
        K_mR = "K_mR",
        K_mS = "K_mS",
        K_mT = "K_mT",
        K_mU = "K_mU",
        K_mV = "K_mV",
        K_mW = "K_mW",
        K_mX = "K_mX",
        K_mY = "K_mY",
        K_mZ = "K_mZ",
        K_BS = "K_BS",
        K_TAB = "K_TAB",
        K_CR = "K_CR",
        K_SP = "K_SP",
        K_ESC = "K_ESC",
        K_sCR = "K_sCR",
        K_sTAB = "K_sTAB",
        K_cTAB = "K_cTAB",
        K_mTAB = "K_mTAB",
        K_HOME = "K_HOME",
        K_UP = "K_UP",
        K_PGUP = "K_PGUP",
        K_LEFT = "K_LEFT",
        K_RIGHT = "K_RIGHT",
        K_END = "K_END",
        K_DOWN = "K_DOWN",
        K_PGDN = "K_PGDN",
        K_MIDDLE = "K_MIDDLE",
        K_INS = "K_INS",
        K_DEL = "K_DEL",
        K_sHOME = "K_sHOME",
        K_sUP = "K_sUP",
        K_sPGUP = "K_sPGUP",
        K_sLEFT = "K_sLEFT",
        K_sRIGHT = "K_sRIGHT",
        K_sEND = "K_sEND",
        K_sDOWN = "K_sDOWN",
        K_sPGDN = "K_sPGDN",
        K_cHOME = "K_cHOME",
        K_cPGUP = "K_cPGUP",
        K_cLEFT = "K_cLEFT",
        K_cRIGHT = "K_cRIGHT",
        K_cEND = "K_cEND",
        K_cPGDN = "K_cPGDN",
        K_cUP = "K_cUP",
        K_cDOWN = "K_cDOWN",
        K_cMIDDLE = "K_cMIDDLE",
        K_cINS = "K_cINS",
        K_cDEL = "K_cDEL",
        K_mHOME = "K_mHOME",
        K_mPGUP = "K_mPGUP",
        K_mLEFT = "K_mLEFT",
        K_mRIGHT = "K_mRIGHT",
        K_mEND = "K_mEND",
        K_mPGDN = "K_mPGDN",
        K_mUP = "K_mUP",
        K_mDOWN = "K_mDOWN",
        K_mINS = "K_mINS",
        K_mDEL = "K_mDEL",
        K_F1 = "K_F1",
        K_F2 = "K_F2",
        K_F3 = "K_F3",
        K_F4 = "K_F4",
        K_F5 = "K_F5",
        K_F6 = "K_F6",
        K_F7 = "K_F7",
        K_F8 = "K_F8",
        K_F9 = "K_F9",
        K_F10 = "K_F10",
        K_F11 = "K_F11",
        K_F12 = "K_F12",
        K_sF1 = "K_sF1",
        K_sF2 = "K_sF2",
        K_sF3 = "K_sF3",
        K_sF4 = "K_sF4",
        K_sF5 = "K_sF5",
        K_sF6 = "K_sF6",
        K_sF7 = "K_sF7",
        K_sF8 = "K_sF8",
        K_sF9 = "K_sF9",
        K_sF10 = "K_sF10",
        K_sF11 = "K_sF11",
        K_sF12 = "K_sF12",
        K_cF1 = "K_cF1",
        K_cF2 = "K_cF2",
        K_cF3 = "K_cF3",
        K_cF4 = "K_cF4",
        K_cF5 = "K_cF5",
        K_cF6 = "K_cF6",
        K_cF7 = "K_cF7",
        K_cF8 = "K_cF8",
        K_cF9 = "K_cF9",
        K_cF10 = "K_cF10",
        K_cF11 = "K_cF11",
        K_cF12 = "K_cF12",
        K_mF1 = "K_mF1",
        K_mF2 = "K_mF2",
        K_mF3 = "K_mF3",
        K_mF4 = "K_mF4",
        K_mF5 = "K_mF5",
        K_mF6 = "K_mF6",
        K_mF7 = "K_mF7",
        K_mF8 = "K_mF8",
        K_mF9 = "K_mF9",
        K_mF10 = "K_mF10",
        K_m1 = "K_m1",
        K_m2 = "K_m2",
        K_m3 = "K_m3",
        K_m4 = "K_m4",
        K_m5 = "K_m5",
        K_m6 = "K_m6",
        K_m7 = "K_m7",
        K_m8 = "K_m8",
        K_m9 = "K_m9",
        K_m0 = "K_m0"
--*/
--public include iupkey.e
/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */
global constant K_SP = ' '   /* 32 (0x20) */
global constant K_exclam = '!' /* 33 */
global constant K_quotedbl = '\"' /* 34 */
global constant K_numbersign = '#' /* 35 */
global constant K_dollar = '$' /* 36 */
global constant K_percent = '%' /* 37 */
global constant K_ampersand = '&' /* 38 */
global constant K_apostrophe = '\'' /* 39 */
global constant K_parentleft = '(' /* 40 */
global constant K_parentright = ')' /* 41 */
global constant K_asterisk = '*' /* 42 */
global constant K_plus = '+' /* 43 */
global constant K_comma = ',' /* 44 */
global constant K_minus = '-' /* 45 */
global constant K_period = '.' /* 46 */
global constant K_slash = '/' /* 47 */
global constant K_0 = '0' /* 48 (0x30) */
global constant K_1 = '1' /* 49 */
global constant K_2 = '2' /* 50 */
global constant K_3 = '3' /* 51 */
global constant K_4 = '4' /* 52 */
global constant K_5 = '5' /* 53 */
global constant K_6 = '6' /* 54 */
global constant K_7 = '7' /* 55 */
global constant K_8 = '8' /* 56 */
global constant K_9 = '9' /* 57 */
global constant K_colon = ':' /* 58 */
global constant K_semicolon = ';' /* 59 */
global constant K_less = '<' /* 60 */
global constant K_equal = '=' /* 61 */
global constant K_greater = '>' /* 62 */
global constant K_question = '?' /* 63 */
global constant K_at = '@' /* 64 */
global constant K_A = 'A' /* 65 (0x41) */
global constant K_B = 'B' /* 66 */
global constant K_C = 'C' /* 67 */
global constant K_D = 'D' /* 68 */
global constant K_E = 'E' /* 69 */
global constant K_F = 'F' /* 70 */
global constant K_G = 'G' /* 71 */
global constant K_H = 'H' /* 72 */
global constant K_I = 'I' /* 73 */
global constant K_J = 'J' /* 74 */
global constant K_K = 'K' /* 75 */
global constant K_L = 'L' /* 76 */
global constant K_M = 'M' /* 77 */
global constant K_N = 'N' /* 78 */
global constant K_O = 'O' /* 79 */
global constant K_P = 'P' /* 80 */
global constant K_Q = 'Q' /* 81 */
global constant K_R = 'R' /* 82 */
global constant K_S = 'S' /* 83 */
global constant K_T = 'T' /* 84 */
global constant K_U = 'U' /* 85 */
global constant K_V = 'V' /* 86 */
global constant K_W = 'W' /* 87 */
global constant K_X = 'X' /* 88 */
global constant K_Y = 'Y' /* 89 */
global constant K_Z = 'Z' /* 90 */
global constant K_bracketleft = '[' /* 91 */
global constant K_backslash = '\\' /* 92 */
global constant K_bracketright = ']' /* 93 */
global constant K_circum = '^' /* 94 */
global constant K_underscore = '_' /* 95 */
global constant K_grave = '`' /* 96 */
global constant K_a = 'a' /* 97 (0x61) */
global constant K_b = 'b' /* 98 */
global constant K_c = 'c' /* 99 */
global constant K_d = 'd' /* 100 */
global constant K_e = 'e' /* 101 */
global constant K_f = 'f' /* 102 */
global constant K_g = 'g' /* 103 */
global constant K_h = 'h' /* 104 */
global constant K_i = 'i' /* 105 */
global constant K_j = 'j' /* 106 */
global constant K_k = 'k' /* 107 */
global constant K_l = 'l' /* 108 */
global constant K_m = 'm' /* 109 */
global constant K_n = 'n' /* 110 */
global constant K_o = 'o' /* 111 */
global constant K_p = 'p' /* 112 */
global constant K_q = 'q' /* 113 */
global constant K_r = 'r' /* 114 */
global constant K_s = 's' /* 115 */
global constant K_t = 't' /* 116 */
global constant K_u = 'u' /* 117 */
global constant K_v = 'v' /* 118 */
global constant K_w = 'w' /* 119 */
global constant K_x = 'x' /* 120 */
global constant K_y = 'y' /* 121 */
global constant K_z = 'z' /* 122 */
global constant K_braceleft = '{' /* 123 */
global constant K_bar = '|' /* 124 */
global constant K_braceright = '}' /* 125 */
global constant K_tilde = '~' /* 126 (0x7E) */
/* Printable ASCii keys */
/* also define the escape sequences that have keys associated */
global constant K_BS = 8 /* 8 */
global constant K_TAB = '\t' /* 9 */
global constant K_LF = '\n' /* 10 (0x0A) not a real key, is a combination of CR with a modifier, just to document */
global constant K_CR = '\r' /* 13 (0x0D) */
/* backward compatible definitions */
global constant K_quoteleft = K_grave
global constant K_quoteright = K_apostrophe
/* IUP Extended Key Codes, range start at 128      */
global constant K_PAUSE = 0xFF13
global constant K_ESC = 0xFF1B
global constant K_HOME = 0xFF50
global constant K_LEFT = 0xFF51
global constant K_UP = 0xFF52
global constant K_RIGHT = 0xFF53
global constant K_DOWN = 0xFF54
global constant K_PGUP = 0xFF55
global constant K_PGDN = 0xFF56
global constant K_END = 0xFF57
global constant K_MIDDLE = 0xFF0B
global constant K_Print = 0xFF61
global constant K_INS = 0xFF63
global constant K_Menu = 0xFF67
global constant K_DEL = 0xFFFF
global constant K_F1 = 0xFFBE
global constant K_F2 = 0xFFBF
global constant K_F3 = 0xFFC0
global constant K_F4 = 0xFFC1
global constant K_F5 = 0xFFC2
global constant K_F6 = 0xFFC3
global constant K_F7 = 0xFFC4
global constant K_F8 = 0xFFC5
global constant K_F9 = 0xFFC6
global constant K_F10 = 0xFFC7
global constant K_F11 = 0xFFC8
global constant K_F12 = 0xFFC9
/* no Shift/Ctrl/Alt */
global constant K_LSHIFT = 0xFFE1
global constant K_RSHIFT = 0xFFE2
global constant K_LCTRL = 0xFFE3
global constant K_RCTRL = 0xFFE4
global constant K_LALT = 0xFFE9
global constant K_RALT = 0xFFEA
global constant K_NUM = 0xFF7F
global constant K_SCROLL = 0xFF14
global constant K_CAPS = 0xFFE5
/* Also, these are the same as the Latin-1 definition */
global constant K_ccedilla = 0x00E7
global constant K_Ccedilla = 0x00C7
global constant K_acute = 0x00B4 /* no Shift/Ctrl/Alt */
global constant K_diaeresis = 0x00A8
/******************************************************/
/* Modifiers use last 4 bits. Since IUP 3.9           */
/* These modifiers definitions are specific to IUP    */
/******************************************************/
global function iup_isShiftXkey( atom _c )
        return and_bits( _c, 0x10000000 )
end function

global function iup_isCtrlXkey( atom _c )
        return and_bits( _c, 0x20000000 )
end function

global function iup_isAltXkey( atom _c )
        return and_bits( _c, 0x40000000 )
end function

global function iup_isSysXkey( atom _c )
        return and_bits( _c, 0x80000000 )
end function

global function iup_XkeyBase( atom _c )
        return and_bits( _c, 0x0FFFFFFF )
end function

global function iup_XkeyShift( atom _c )
        return or_bits( _c, 0x10000000 )
end function

global function iup_XkeyCtrl( atom _c )
        return or_bits( _c, 0x20000000 )
end function

global function iup_XkeyAlt( atom _c )
        return or_bits( _c, 0x40000000 )
end function

global function iup_XkeySys( atom _c )
        return or_bits( _c, 0x80000000 )
end function

global constant K_sHOME = iup_XkeyShift(K_HOME )
global constant K_sUP = iup_XkeyShift(K_UP )
global constant K_sPGUP = iup_XkeyShift(K_PGUP )
global constant K_sLEFT = iup_XkeyShift(K_LEFT )
global constant K_sMIDDLE = iup_XkeyShift(K_MIDDLE )
global constant K_sRIGHT = iup_XkeyShift(K_RIGHT )
global constant K_sEND = iup_XkeyShift(K_END )
global constant K_sDOWN = iup_XkeyShift(K_DOWN )
global constant K_sPGDN = iup_XkeyShift(K_PGDN )
global constant K_sINS = iup_XkeyShift(K_INS )
global constant K_sDEL = iup_XkeyShift(K_DEL )
global constant K_sSP = iup_XkeyShift(K_SP )
global constant K_sTAB = iup_XkeyShift(K_TAB )
global constant K_sCR = iup_XkeyShift(K_CR )
global constant K_sBS = iup_XkeyShift(K_BS )
global constant K_sPAUSE = iup_XkeyShift(K_PAUSE )
global constant K_sESC = iup_XkeyShift(K_ESC )
global constant K_sF1 = iup_XkeyShift(K_F1 )
global constant K_sF2 = iup_XkeyShift(K_F2 )
global constant K_sF3 = iup_XkeyShift(K_F3 )
global constant K_sF4 = iup_XkeyShift(K_F4 )
global constant K_sF5 = iup_XkeyShift(K_F5 )
global constant K_sF6 = iup_XkeyShift(K_F6 )
global constant K_sF7 = iup_XkeyShift(K_F7 )
global constant K_sF8 = iup_XkeyShift(K_F8 )
global constant K_sF9 = iup_XkeyShift(K_F9 )
global constant K_sF10 = iup_XkeyShift(K_F10 )
global constant K_sF11 = iup_XkeyShift(K_F11 )
global constant K_sF12 = iup_XkeyShift(K_F12 )
global constant K_sPrint = iup_XkeyShift(K_Print )
global constant K_sMenu = iup_XkeyShift(K_Menu )
global constant K_cHOME = iup_XkeyCtrl(K_HOME )
global constant K_cUP = iup_XkeyCtrl(K_UP )
global constant K_cPGUP = iup_XkeyCtrl(K_PGUP )
global constant K_cLEFT = iup_XkeyCtrl(K_LEFT )
global constant K_cMIDDLE = iup_XkeyCtrl(K_MIDDLE )
global constant K_cRIGHT = iup_XkeyCtrl(K_RIGHT )
global constant K_cEND = iup_XkeyCtrl(K_END )
global constant K_cDOWN = iup_XkeyCtrl(K_DOWN )
global constant K_cPGDN = iup_XkeyCtrl(K_PGDN )
global constant K_cINS = iup_XkeyCtrl(K_INS )
global constant K_cDEL = iup_XkeyCtrl(K_DEL )
global constant K_cSP = iup_XkeyCtrl(K_SP )
global constant K_cTAB = iup_XkeyCtrl(K_TAB )
global constant K_cCR = iup_XkeyCtrl(K_CR )
global constant K_cBS = iup_XkeyCtrl(K_BS )
global constant K_cPAUSE = iup_XkeyCtrl(K_PAUSE )
global constant K_cESC = iup_XkeyCtrl(K_ESC )
global constant K_cCcedilla = iup_XkeyCtrl(K_Ccedilla)
global constant K_cF1 = iup_XkeyCtrl(K_F1 )
global constant K_cF2 = iup_XkeyCtrl(K_F2 )
global constant K_cF3 = iup_XkeyCtrl(K_F3 )
global constant K_cF4 = iup_XkeyCtrl(K_F4 )
global constant K_cF5 = iup_XkeyCtrl(K_F5 )
global constant K_cF6 = iup_XkeyCtrl(K_F6 )
global constant K_cF7 = iup_XkeyCtrl(K_F7 )
global constant K_cF8 = iup_XkeyCtrl(K_F8 )
global constant K_cF9 = iup_XkeyCtrl(K_F9 )
global constant K_cF10 = iup_XkeyCtrl(K_F10 )
global constant K_cF11 = iup_XkeyCtrl(K_F11 )
global constant K_cF12 = iup_XkeyCtrl(K_F12 )
global constant K_cPrint = iup_XkeyCtrl(K_Print )
global constant K_cMenu = iup_XkeyCtrl(K_Menu )
global constant K_mHOME = iup_XkeyAlt(K_HOME )
global constant K_mUP = iup_XkeyAlt(K_UP )
global constant K_mPGUP = iup_XkeyAlt(K_PGUP )
global constant K_mLEFT = iup_XkeyAlt(K_LEFT )
global constant K_mMIDDLE = iup_XkeyAlt(K_MIDDLE )
global constant K_mRIGHT = iup_XkeyAlt(K_RIGHT )
global constant K_mEND = iup_XkeyAlt(K_END )
global constant K_mDOWN = iup_XkeyAlt(K_DOWN )
global constant K_mPGDN = iup_XkeyAlt(K_PGDN )
global constant K_mINS = iup_XkeyAlt(K_INS )
global constant K_mDEL = iup_XkeyAlt(K_DEL )
global constant K_mSP = iup_XkeyAlt(K_SP )
global constant K_mTAB = iup_XkeyAlt(K_TAB )
global constant K_mCR = iup_XkeyAlt(K_CR )
global constant K_mBS = iup_XkeyAlt(K_BS )
global constant K_mPAUSE = iup_XkeyAlt(K_PAUSE )
global constant K_mESC = iup_XkeyAlt(K_ESC )
global constant K_mCcedilla = iup_XkeyAlt(K_Ccedilla)
global constant K_mF1 = iup_XkeyAlt(K_F1 )
global constant K_mF2 = iup_XkeyAlt(K_F2 )
global constant K_mF3 = iup_XkeyAlt(K_F3 )
global constant K_mF4 = iup_XkeyAlt(K_F4 )
global constant K_mF5 = iup_XkeyAlt(K_F5 )
global constant K_mF6 = iup_XkeyAlt(K_F6 )
global constant K_mF7 = iup_XkeyAlt(K_F7 )
global constant K_mF8 = iup_XkeyAlt(K_F8 )
global constant K_mF9 = iup_XkeyAlt(K_F9 )
global constant K_mF10 = iup_XkeyAlt(K_F10 )
global constant K_mF11 = iup_XkeyAlt(K_F11 )
global constant K_mF12 = iup_XkeyAlt(K_F12 )
global constant K_mPrint = iup_XkeyAlt(K_Print )
global constant K_mMenu = iup_XkeyAlt(K_Menu )
global constant K_yHOME = iup_XkeySys(K_HOME )
global constant K_yUP = iup_XkeySys(K_UP )
global constant K_yPGUP = iup_XkeySys(K_PGUP )
global constant K_yLEFT = iup_XkeySys(K_LEFT )
global constant K_yMIDDLE = iup_XkeySys(K_MIDDLE )
global constant K_yRIGHT = iup_XkeySys(K_RIGHT )
global constant K_yEND = iup_XkeySys(K_END )
global constant K_yDOWN = iup_XkeySys(K_DOWN )
global constant K_yPGDN = iup_XkeySys(K_PGDN )
global constant K_yINS = iup_XkeySys(K_INS )
global constant K_yDEL = iup_XkeySys(K_DEL )
global constant K_ySP = iup_XkeySys(K_SP )
global constant K_yTAB = iup_XkeySys(K_TAB )
global constant K_yCR = iup_XkeySys(K_CR )
global constant K_yBS = iup_XkeySys(K_BS )
global constant K_yPAUSE = iup_XkeySys(K_PAUSE )
global constant K_yESC = iup_XkeySys(K_ESC )
global constant K_yCcedilla = iup_XkeySys(K_Ccedilla)
global constant K_yF1 = iup_XkeySys(K_F1 )
global constant K_yF2 = iup_XkeySys(K_F2 )
global constant K_yF3 = iup_XkeySys(K_F3 )
global constant K_yF4 = iup_XkeySys(K_F4 )
global constant K_yF5 = iup_XkeySys(K_F5 )
global constant K_yF6 = iup_XkeySys(K_F6 )
global constant K_yF7 = iup_XkeySys(K_F7 )
global constant K_yF8 = iup_XkeySys(K_F8 )
global constant K_yF9 = iup_XkeySys(K_F9 )
global constant K_yF10 = iup_XkeySys(K_F10 )
global constant K_yF11 = iup_XkeySys(K_F11 )
global constant K_yF12 = iup_XkeySys(K_F12 )
global constant K_yPrint = iup_XkeySys(K_Print )
global constant K_yMenu = iup_XkeySys(K_Menu )
global constant K_sPlus = iup_XkeyShift(K_plus )
global constant K_sComma = iup_XkeyShift(K_comma )
global constant K_sMinus = iup_XkeyShift(K_minus )
global constant K_sPeriod = iup_XkeyShift(K_period )
global constant K_sSlash = iup_XkeyShift(K_slash )
global constant K_sAsterisk = iup_XkeyShift(K_asterisk)
global constant K_cA = iup_XkeyCtrl(K_A)
global constant K_cB = iup_XkeyCtrl(K_B)
global constant K_cC = iup_XkeyCtrl(K_C)
global constant K_cD = iup_XkeyCtrl(K_D)
global constant K_cE = iup_XkeyCtrl(K_E)
global constant K_cF = iup_XkeyCtrl(K_F)
global constant K_cG = iup_XkeyCtrl(K_G)
global constant K_cH = iup_XkeyCtrl(K_H)
global constant K_cI = iup_XkeyCtrl(K_I)
global constant K_cJ = iup_XkeyCtrl(K_J)
global constant K_cK = iup_XkeyCtrl(K_K)
global constant K_cL = iup_XkeyCtrl(K_L)
global constant K_cM = iup_XkeyCtrl(K_M)
global constant K_cN = iup_XkeyCtrl(K_N)
global constant K_cO = iup_XkeyCtrl(K_O)
global constant K_cP = iup_XkeyCtrl(K_P)
global constant K_cQ = iup_XkeyCtrl(K_Q)
global constant K_cR = iup_XkeyCtrl(K_R)
global constant K_cS = iup_XkeyCtrl(K_S)
global constant K_cT = iup_XkeyCtrl(K_T)
global constant K_cU = iup_XkeyCtrl(K_U)
global constant K_cV = iup_XkeyCtrl(K_V)
global constant K_cW = iup_XkeyCtrl(K_W)
global constant K_cX = iup_XkeyCtrl(K_X)
global constant K_cY = iup_XkeyCtrl(K_Y)
global constant K_cZ = iup_XkeyCtrl(K_Z)
global constant K_c1 = iup_XkeyCtrl(K_1)
global constant K_c2 = iup_XkeyCtrl(K_2)
global constant K_c3 = iup_XkeyCtrl(K_3)
global constant K_c4 = iup_XkeyCtrl(K_4)
global constant K_c5 = iup_XkeyCtrl(K_5)
global constant K_c6 = iup_XkeyCtrl(K_6)
global constant K_c7 = iup_XkeyCtrl(K_7)
global constant K_c8 = iup_XkeyCtrl(K_8)
global constant K_c9 = iup_XkeyCtrl(K_9)
global constant K_c0 = iup_XkeyCtrl(K_0)
global constant K_cPlus = iup_XkeyCtrl(K_plus )
global constant K_cComma = iup_XkeyCtrl(K_comma )
global constant K_cMinus = iup_XkeyCtrl(K_minus )
global constant K_cPeriod = iup_XkeyCtrl(K_period )
global constant K_cSlash = iup_XkeyCtrl(K_slash )
global constant K_cSemicolon = iup_XkeyCtrl(K_semicolon )
global constant K_cEqual = iup_XkeyCtrl(K_equal )
global constant K_cBracketleft = iup_XkeyCtrl(K_bracketleft )
global constant K_cBracketright = iup_XkeyCtrl(K_bracketright)
global constant K_cBackslash = iup_XkeyCtrl(K_backslash )
global constant K_cAsterisk = iup_XkeyCtrl(K_asterisk )
global constant K_mA = iup_XkeyAlt(K_A)
global constant K_mB = iup_XkeyAlt(K_B)
global constant K_mC = iup_XkeyAlt(K_C)
global constant K_mD = iup_XkeyAlt(K_D)
global constant K_mE = iup_XkeyAlt(K_E)
global constant K_mF = iup_XkeyAlt(K_F)
global constant K_mG = iup_XkeyAlt(K_G)
global constant K_mH = iup_XkeyAlt(K_H)
global constant K_mI = iup_XkeyAlt(K_I)
global constant K_mJ = iup_XkeyAlt(K_J)
global constant K_mK = iup_XkeyAlt(K_K)
global constant K_mL = iup_XkeyAlt(K_L)
global constant K_mM = iup_XkeyAlt(K_M)
global constant K_mN = iup_XkeyAlt(K_N)
global constant K_mO = iup_XkeyAlt(K_O)
global constant K_mP = iup_XkeyAlt(K_P)
global constant K_mQ = iup_XkeyAlt(K_Q)
global constant K_mR = iup_XkeyAlt(K_R)
global constant K_mS = iup_XkeyAlt(K_S)
global constant K_mT = iup_XkeyAlt(K_T)
global constant K_mU = iup_XkeyAlt(K_U)
global constant K_mV = iup_XkeyAlt(K_V)
global constant K_mW = iup_XkeyAlt(K_W)
global constant K_mX = iup_XkeyAlt(K_X)
global constant K_mY = iup_XkeyAlt(K_Y)
global constant K_mZ = iup_XkeyAlt(K_Z)
global constant K_m1 = iup_XkeyAlt(K_1)
global constant K_m2 = iup_XkeyAlt(K_2)
global constant K_m3 = iup_XkeyAlt(K_3)
global constant K_m4 = iup_XkeyAlt(K_4)
global constant K_m5 = iup_XkeyAlt(K_5)
global constant K_m6 = iup_XkeyAlt(K_6)
global constant K_m7 = iup_XkeyAlt(K_7)
global constant K_m8 = iup_XkeyAlt(K_8)
global constant K_m9 = iup_XkeyAlt(K_9)
global constant K_m0 = iup_XkeyAlt(K_0)
global constant K_mPlus = iup_XkeyAlt(K_plus )
global constant K_mComma = iup_XkeyAlt(K_comma )
global constant K_mMinus = iup_XkeyAlt(K_minus )
global constant K_mPeriod = iup_XkeyAlt(K_period )
global constant K_mSlash = iup_XkeyAlt(K_slash )
global constant K_mSemicolon = iup_XkeyAlt(K_semicolon )
global constant K_mEqual = iup_XkeyAlt(K_equal )
global constant K_mBracketleft = iup_XkeyAlt(K_bracketleft )
global constant K_mBracketright = iup_XkeyAlt(K_bracketright)
global constant K_mBackslash = iup_XkeyAlt(K_backslash )
global constant K_mAsterisk = iup_XkeyAlt(K_asterisk )
global constant K_yA = iup_XkeySys(K_A)
global constant K_yB = iup_XkeySys(K_B)
global constant K_yC = iup_XkeySys(K_C)
global constant K_yD = iup_XkeySys(K_D)
global constant K_yE = iup_XkeySys(K_E)
global constant K_yF = iup_XkeySys(K_F)
global constant K_yG = iup_XkeySys(K_G)
global constant K_yH = iup_XkeySys(K_H)
global constant K_yI = iup_XkeySys(K_I)
global constant K_yJ = iup_XkeySys(K_J)
global constant K_yK = iup_XkeySys(K_K)
global constant K_yL = iup_XkeySys(K_L)
global constant K_yM = iup_XkeySys(K_M)
global constant K_yN = iup_XkeySys(K_N)
global constant K_yO = iup_XkeySys(K_O)
global constant K_yP = iup_XkeySys(K_P)
global constant K_yQ = iup_XkeySys(K_Q)
global constant K_yR = iup_XkeySys(K_R)
global constant K_yS = iup_XkeySys(K_S)
global constant K_yT = iup_XkeySys(K_T)
global constant K_yU = iup_XkeySys(K_U)
global constant K_yV = iup_XkeySys(K_V)
global constant K_yW = iup_XkeySys(K_W)
global constant K_yX = iup_XkeySys(K_X)
global constant K_yY = iup_XkeySys(K_Y)
global constant K_yZ = iup_XkeySys(K_Z)
global constant K_y1 = iup_XkeySys(K_1)
global constant K_y2 = iup_XkeySys(K_2)
global constant K_y3 = iup_XkeySys(K_3)
global constant K_y4 = iup_XkeySys(K_4)
global constant K_y5 = iup_XkeySys(K_5)
global constant K_y6 = iup_XkeySys(K_6)
global constant K_y7 = iup_XkeySys(K_7)
global constant K_y8 = iup_XkeySys(K_8)
global constant K_y9 = iup_XkeySys(K_9)
global constant K_y0 = iup_XkeySys(K_0)
global constant K_yPlus = iup_XkeySys(K_plus )
global constant K_yComma = iup_XkeySys(K_comma )
global constant K_yMinus = iup_XkeySys(K_minus )
global constant K_yPeriod = iup_XkeySys(K_period )
global constant K_ySlash = iup_XkeySys(K_slash )
global constant K_ySemicolon = iup_XkeySys(K_semicolon )
global constant K_yEqual = iup_XkeySys(K_equal )
global constant K_yBracketleft = iup_XkeySys(K_bracketleft )
global constant K_yBracketright = iup_XkeySys(K_bracketright)
global constant K_yBackslash = iup_XkeySys(K_backslash )
global constant K_yAsterisk = iup_XkeySys(K_asterisk )


--include defs.e


--DEV PL:
--/**/  global procedure free_pointer_array(atom pointers_array)
--/**/  atom next_ptr = pointers_array, ptr
--/**/      while 1 do
--/**/          ptr = peek4u(next_ptr)
--/**/          if ptr=0 then exit end if
--/**/          free(ptr)
--/**/          next_ptr += 4
--/**/      end while
--/**/      free(pointers_array)
--/**/  end procedure
--/**/  constant FREE_ARRAY_RID = routine_id("free_pointer_array")
--/**/  
--/**/  global function allocate_pointer_array(sequence pointers, integer cleanup = 0)
--/**/  atom pList
--/**/  
--/**/      pointers &= 0
--/**/      if machine_bits()=32 then
--/**/          pList = allocate(length(pointers)*4)
--/**/          poke4(pList, pointers)
--/**/      else
--/**/          pList = allocate(length(pointers)*8)
--/**/          poke8(pList, pointers)
--/**/      end if
--/**/      if cleanup then
--/**/          return delete_routine(pList, FREE_ARRAY_RID)
--/**/      end if
--/**/      return pList
--/**/  end function
--/**/  
--/**/  global function rand_range(integer lo, integer hi)
--/**/  integer temp
--/**/  
--/**/      if lo>hi then
--/**/          temp = hi
--/**/          hi = lo
--/**/          lo = temp
--/**/      end if
--/**/  
--/**/      lo -= 1
--/**/      hi -= lo
--/**/  
--/**/      return lo+rand(hi)
--/**/  end function
--/**/  
--/**/  include builtins\read_file.e

--SUG:
integer libidx = -1
--global 
function iup_open_dll(sequence libs)
atom res
    if libidx=-1 then
        for i=1 to length(libs) do
            res = open_dll(libs[i])
            if res!=0 then
                libidx = i
                exit
            end if
        end for
    else
        res = open_dll(libs[libidx])
    end if
-- erm: this is probably better off being in iup_open()...
--  (that way you can call has_iup() and use something else?)
    if res=0 then
        puts(1,"error opening ")
        if libidx then
            puts(1,libs[libidx])
        else
            ?libs
        end if
        {} = wait_key()
        ?9/0
    end if
    return res
end function


--include common.e
--DEV** compiler error when this was "version", probably because there are/were several global version() knocking about...
global constant common_version = "3.3.0p1"

--global 
atom
        --**
        -- iup.dll handle
        hIup,

        --**
        -- Pointer to the current language (if changed)
        pLanguage=0

--global type Ihandle(atom o)
global type Ihandle(object o)
--  if atom(o) then end if
--  return 1
    return atom(o)
end type

--include core.e
--include wrap.e
--global 
constant
    UC = C_UCHAR,
    P  = C_POINTER, 
    F  = C_FLOAT, 
    D  = C_DOUBLE, 
    I  = C_INT,
    L  = C_LONG,
    UL = C_ULONG


--global 
procedure poke_double(atom ptr, object data)
    if atom(data) then
        poke(ptr,atom_to_float64(data))
    else
        for i=1 to length(data) do
            poke(ptr+8*(i-1),atom_to_float64(data[i]))
        end for
    end if
end procedure

--global 
function peek_double(object pDouble)
sequence doubles

    if atom(pDouble) then
        return float64_to_atom(peek({pDouble,8}))
    else
        doubles = {}

        for i=1 to pDouble[2] do
            doubles &= float64_to_atom(peek({pDouble[1]+8*(i-1),8}))
        end for

        return doubles
    end if
end function

--global 
function get_string_array(integer hFunc, integer max_n, object classname = {})
sequence name_list
sequence plist          -- list of pointers to above
atom array_pointer      -- pointer to above array
integer name_count      -- the number of names loaded to the table
--integer inc
--atom temp

    name_list = {}
    array_pointer = 0

    -- create an area of memory for the pointer to the array to be reurned to
    array_pointer = allocate(4*max_n)

    if atom(classname) then
        name_count = c_func(hFunc, {classname, array_pointer, max_n})
--  elsif sequence(classname) and length(classname) then
    elsif length(classname) then
        atom hClassname = allocate_string(classname)
        name_count = c_func(hFunc, {hClassname, array_pointer, max_n})
        free(hClassname)
    else
        name_count = c_func(hFunc, {array_pointer, max_n})
    end if

    -- get the list of pointers for the strings
    plist = {}
    plist = peek4u({array_pointer, name_count})

    -- get the list of strings
    name_list = repeat({}, name_count)
    for i=1 to name_count do
        name_list[i] = peek_string(plist[i])
    end for

    free(array_pointer)

    return name_list
end function

--include common.e

constant iupdlls = {
                    "iup.dll",
                    "libiup.so",
                    "libiup.dylib"
                   }
hIup = iup_open_dll(iupdlls)

--****
-- === Routines
--

constant
    xIupOpen = define_c_func(hIup, "IupOpen", {I,P},I),
    xIupClose = define_c_proc(hIup, "IupClose", {}),
    xIupVersion = define_c_func(hIup, "IupVersion", {},P),
    xIupLoad = define_c_func(hIup, "IupLoad", {P},P),
    xIupLoadBuffer = define_c_func(hIup, "IupLoadBuffer", {P},P),
    xIupSetLanguage = define_c_proc(hIup, "IupSetLanguage", {P}),
    xIupGetLanguage = define_c_func(hIup, "IupGetLanguage", {},P)

--**
-- Does this system have access to the Iup DLL files?
--
-- Returns:
--   TRUE or FALSE
--

global function has_iup()
    return hIup!=0
end function

global function IupOpen()
    if hIup=0 then
        printf(1,"error opening %s/%s/%s",iupdlls)
        {} = wait_key()
        ?9/0
    end if
    return c_func(xIupOpen,{0,0})
end function

global procedure IupClose()
    c_proc(xIupClose)
end procedure

global function iup_version()
    -- static value internal to iup, do not need to free
    return peek_string(c_func(xIupVersion))
end function

global function iup_load(sequence filename)
atom pFilename = allocate_string(filename)
atom pError = c_func(xIupLoad, {pFilename})
    free(pFilename)
    if pError!=0 then
        return peek_string(pError)
    end if
    return 1
end function

global function load_buffer(sequence buffer)
atom pBuffer = allocate_string(buffer)
atom pError = c_func(xIupLoadBuffer, {pBuffer})
    free(pBuffer)
    if pError!=0 then
        return peek_string(pError)
    end if
    return 1
end function

global procedure IupSetLanguage(sequence language)
    if pLanguage then
        free(pLanguage)
    end if

    pLanguage = allocate_string(language)

    c_proc(xIupSetLanguage, {pLanguage})
end procedure

global function get_language()
    return peek_string(c_func(xIupGetLanguage, {}))
end function

--include attributes.e
constant
    xIupStoreAttribute = define_c_proc(hIup, "IupStoreAttribute", {P,P,P}),
    xIupSetAttribute = define_c_proc(hIup, "IupSetAttribute", {P,P,P}),
    xIupSetAttributes = define_c_proc(hIup, "IupSetAttributes", {P,P}),
    xIupSetInt = define_c_proc(hIup, "IupSetInt", {P,P,I}),
    xIupResetAttribute = define_c_proc(hIup, "IupResetAttribute", {P,P}),
    xIupGetAttribute = define_c_func(hIup, "IupGetAttribute", {P,P},P),
    xIupGetAllAttributes = define_c_func(hIup, "IupGetAllAttributes", {P,P,I},I),
    xIupSetAttributeHandle = define_c_proc(hIup, "IupSetAttributeHandle", {P,P,P}),
    xIupGetAttributeHandle = define_c_func(hIup, "IupGetAttributeHandle", {P,P},P),
    xIupGetAttributes = define_c_func(hIup, "IupGetAttributes", {P},P),
    xIupGetFloat = define_c_func(hIup, "IupGetFloat", {P,P},F),
    xIupGetInt = define_c_func(hIup, "IupGetInt", {P,P},I),
    xIupGetInt2 = define_c_func(hIup, "IupGetInt", {P,P},I),
    xIupStoreGlobal = define_c_proc(hIup, "IupStoreGlobal", {P,P}),
    xIupSetGlobal = define_c_proc(hIup, "IupSetGlobal", {P,P}),
    xIupGetGlobal = define_c_func(hIup, "IupGetGlobal", {P},P)

--**
-- Store an attribute
--
-- Parameters:
--   * ##ih## - Widget handle
--   * ##name## - Attribute to set
--   * ##value## - Value of attribute
--   * ##data## - Optional data for ##sprintf()## format of value
-- 
-- Notes:
--   Varies from IupStoreAttribute in that it can accept ##sprintf()## type formatting via
--   the ##value## and ##data## parameters.
--

global procedure IupStoreAttribute(Ihandle ih, sequence name, object value, sequence data = {})
atom pName = allocate_string(name), pValue = 0

    if length(data) then
        value = sprintf(value, data)
    end if

    if sequence(value) then
        pValue = allocate_string(value)
    else
        pValue = value
    end if

    c_proc(xIupStoreAttribute, {ih, pName, pValue})

    free(pName)

    if sequence(value) then
        free(pValue)
    end if
end procedure

global procedure IupSetAttribute(Ihandle ih, sequence name, object pValue)
atom pName = allocate_string(name,1)
    c_proc(xIupSetAttribute, {ih, pName, pValue})
end procedure

global procedure IupSetStrAttribute(Ihandle ih, sequence name, object pValue)
    IupSetAttribute(ih, name, pValue)
end procedure

global procedure IupSetAttributef(Ihandle ih, sequence name, string fmt, sequence data)
atom pName = allocate_string(name,1)
atom pValue = allocate_string(sprintf(fmt,data),1)
    c_proc(xIupSetAttribute, {ih, pName, pValue})
end procedure

--**
-- For naming only, please use [[:store_attribute]] instead
--

global procedure setf_attribute(Ihandle ih, sequence attribute, sequence data = {})
    IupSetAttributes(ih, attribute, data)
end procedure

--**
-- Set many attributes for a given widget
--
-- Parameters:
--   * ##ih## - Widget handle
--   * ##attributes## - Attributes to set
--   * ##data## - Optional data to format ##attributes## via ##sprintf()## if needed.
--
-- Notes:
--   Varies from IupStoreAttribute in that it can accept ##sprintf()## type formatting via
--   the ##value## and ##data## parameters.
--

global procedure IupSetAttributes(Ihandle ih, sequence attributes, sequence data = {})
atom pAttributes

    if length(data) then
        attributes = sprintf(attributes, data)
    end if

    pAttributes = allocate_string(attributes)

    c_proc(xIupSetAttributes, {ih, pAttributes})

    free(pAttributes)
end procedure

global function IupSetAttributesf(Ihandle ih, sequence attributes, sequence data = {})
    IupSetAttributes(ih, attributes, data)
    return ih
end function

global procedure reset_attribute(Ihandle ih, sequence name)
atom pName = allocate_string(name)
    c_proc(xIupResetAttribute, {ih, pName})
    free(pName)
end procedure

global function set_att(sequence handleName, Ihandle ih, sequence values)
sequence attributes

    for i=1 to length(values) by 2 do
        attributes &= sprintf(`%s="%s"`, {values[i], values[i+1]})
    end for

    if length(handleName) then
        -- TODO: HANDLE via IupSetHandle()
    end if

    IupSetAttributes(ih, attributes)

    return ih
end function

global procedure IupSetAttributeHandle(Ihandle ih, sequence name, Ihandle ih_named)
atom pName = allocate_string(name)
    c_proc(xIupSetAttributeHandle, {ih, pName, ih_named})
    free(pName)
end procedure

--void IupSetInt(Ihandle* ih, const char* name, int v);
global procedure IupSetInt(atom ih, object name, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetInt, {ih,name,v})
end procedure

global function IupGetAttribute(Ihandle ih, sequence name)
atom pName = allocate_string(name)

atom pValue = c_func(xIupGetAttribute, {ih, pName})

    free(pName)
    if pValue=NULL then return NULL end if
    return peek_string(pValue)
end function

global function get_all_attributes(Ihandle ih, integer max_n = 128)
    return get_string_array(xIupGetAllAttributes, max_n, ih)
end function

global function get_attribute_handle(Ihandle ih, sequence name)
atom pName = allocate_string(name)

Ihandle ih_named = c_func(xIupGetAttributeHandle, {ih, pName})

    free(pName)

    return ih_named
end function

global function get_attributes(Ihandle ih)
atom pAttributes = c_func(xIupGetAttributes, {ih})
    return peek_string(pAttributes)
end function

global function get_float(Ihandle ih, sequence name)
atom pName = allocate_string(name), pValue

    pValue = c_func(xIupGetFloat, {ih, pName})

    free(pName)

    return atom_to_float32(pValue)
end function

global function IupGetInt(Ihandle ih, sequence name)
atom pName = allocate_string(name)
integer v = c_func(xIupGetInt, {ih, pName})
    free(pName)
    return v
end function

global function IupGetInt2(Ihandle ih, sequence name)
atom pName = allocate_string(name)
integer v = c_func(xIupGetInt2, {ih, pName})
    free(pName)
    return v
end function

global procedure store_global(sequence name, object value)
atom pName = allocate_string(name), pValue = value

    if sequence(value) then
        pValue = allocate_string(value)
    end if

    c_proc(xIupStoreGlobal, {pName, pValue})

    if sequence(value) then
        free(pValue)
    end if

    free(pName)
end procedure

global procedure IupSetGlobal(object name, object v)
    if string(name) then name = allocate_string(name,1) end if
    if string(v) then v = allocate_string(v,1) end if
    c_proc(xIupSetGlobal, {name, v})
end procedure

global function IupGetGlobal(sequence name)
atom pName = allocate_string(name)
atom pValue = c_func(xIupGetGlobal, {pName})
    free(pName)
    return peek_string(pValue)
end function

--include events.e
constant
    xIupMainLoop = define_c_proc(hIup, "IupMainLoop", {}),
    xIupMainLoopLevel = define_c_func(hIup, "IupMainLoopLevel", {},I),
    xIupLoopStep = define_c_proc(hIup, "IupLoopStep", {}),
    xIupExitLoop = define_c_proc(hIup, "IupExitLoop", {}),
    xIupFlush = define_c_proc(hIup, "IupFlush", {}),
    xIupRecordInput = define_c_func(hIup, "IupRecordInput", {P,I},I),
    xIupPlayInput = define_c_func(hIup, "IupPlayInput", {P},I),
    xIupGetCallback = define_c_func(hIup, "IupGetCallback", {P,P},P),
    xIupSetCallback = define_c_func(hIup, "IupSetCallback", {P,P,P},P),
    xIupGetActionName = define_c_func(hIup, "IupGetActionName", {},P)

global procedure IupMainLoop()
    c_proc(xIupMainLoop, {})
end procedure

global function main_loop_level()
    return c_func(xIupMainLoopLevel, {})
end function

global procedure loop_step()
    c_proc(xIupLoopStep, {})
end procedure

global procedure exit_loop()
    c_proc(xIupExitLoop, {})
end procedure

global procedure flush_events()
    c_proc(xIupFlush, {})
end procedure

--int IupRecordInput(const char* filename, int mode);
global procedure IupRecordInput(object filename, atom mode)
    if sequence(filename) then filename = allocate_string(filename,1) end if
    atom result = c_func(xIupRecordInput, {filename,mode})
--  --free(filename)?
--  return result
end procedure

--int IupPlayInput(const char* filename);
global procedure IupPlayInput(object filename = NULL)
    if sequence(filename) then filename = allocate_string(filename,1) end if
    atom result = c_func(xIupPlayInput, {filename})
--  return result
end procedure


global function get_callback(Ihandle ih, sequence name)
atom pName = allocate_string(name)
atom pCallback = c_func(xIupGetCallback, {ih, pName})
    free(pName)
    return pCallback
end function

global procedure IupSetCallback(Ihandle ih, object name, integer rid)
atom pName
    if sequence(name) then
        pName = allocate_string(name)
    else
        pName = name
    end if

    {} = c_func(xIupSetCallback, {ih, pName, call_back({'+', rid})})

    if sequence(name) then
        free(pName)
    end if
end procedure

global function IupSetCallbackf(Ihandle ih, object name, integer rid)
    IupSetCallback(ih, name, rid)
    return ih
end function

global procedure set_events_callbacks(Ihandle ih, sequence name, sequence funcs)
atom pName = allocate_string(name)

    for i=1 to length(funcs) do
        IupSetCallback(ih, pName, funcs[i])
    end for

    free(pName)
end procedure

global function get_action_name()
atom pValue = c_func(xIupGetActionName, {})
    return peek_string(pValue)
end function

--include layout.e
constant
    xIupCreate = define_c_func(hIup, "IupCreate", {P},P),
    xIupDestroy = define_c_proc(hIup, "IupDestroy", {P}),
    xIupMap = define_c_func(hIup, "IupMap", {P},I),
    xIupUnmap = define_c_proc(hIup, "IupUnmap", {P})

global function create(sequence name)
atom pName = allocate_string(name)
Ihandle pHandle = c_func(xIupCreate, {pName})
    free(pName)
    return pHandle
end function

global procedure IupDestroy(Ihandle ih)
    c_proc(xIupDestroy, {ih})
end procedure

global function IupMap(Ihandle ih)
    return c_func(xIupMap, {ih})
end function

global procedure iup_unmap(Ihandle ih)
    c_proc(xIupUnmap, {ih})
end procedure

--****
-- === Class Information
--

constant
    xIupGetAllClasses = define_c_func(hIup, "IupGetAllClasses", {P,I},I),
    xIupGetClassName = define_c_func(hIup, "IupGetClassName", {P},P),
    xIupGetClassType = define_c_func(hIup, "IupGetClassType", {P},P),
    xIupGetClassAttributes = define_c_func(hIup, "IupGetClassAttributes", {P,P,I},I),
    xIupGetClassCallbacks = define_c_func(hIup, "IupGetClassCallbacks", {P,P,I},I),
    xIupSaveClassAttributes = define_c_proc(hIup, "IupSaveClassAttributes", {P}),
    xIupCopyClassAttributes = define_c_proc(hIup, "IupCopyClassAttributes", {P,P}),
    xIupSetClassDefaultAttribute = define_c_proc(hIup, "IupSetClassDefaultAttribute", {P,P,P})

--**
-- **Not yet implemented**
--

global function get_all_classes(integer max_n = 128)
    return get_string_array(xIupGetAllClasses, max_n)
end function

global function IupGetClassName(Ihandle ih)
atom hResult = c_func(xIupGetClassName, {ih})
    return peek_string(hResult)
end function

global function get_class_type(Ihandle ih)
atom hResult = c_func(xIupGetClassType, {ih})
    return peek_string(hResult)
end function

global function get_class_attributes(sequence classname, integer max_n = 128)
    return get_string_array(xIupGetClassAttributes, max_n, classname)
end function

global function get_class_callbacks(sequence classname, integer max_n = 128)
    return get_string_array(xIupGetClassCallbacks, max_n, classname)
end function

global procedure save_class_attributes(Ihandle ih)
    c_proc(xIupSaveClassAttributes, {ih})
end procedure

global procedure copy_class_attributes(Ihandle src_ih, Ihandle dst_ih)
    c_proc(xIupCopyClassAttributes, {src_ih, dst_ih})
end procedure

global procedure set_class_default_attribute(sequence classname, sequence name, sequence value)
atom hClassname = allocate_string(classname)
atom hName = allocate_string(name)
atom hValue = allocate_string(value)

    c_proc(xIupSetClassDefaultAttribute, {hClassname, hName, hValue})

    free(hClassname)
    free(hName)
    free(hValue)
end procedure

--****
-- === Layout/Composition
--

constant
    xIupFill  = define_c_func(hIup, "IupFill", {},P),
    xIupHboxv = define_c_func(hIup, "IupHboxv", {P},P),
    xIupVboxv = define_c_func(hIup, "IupVboxv", {P},P),
    xIupZboxv = define_c_func(hIup, "IupZboxv", {P},P),
    xIupRadio = define_c_func(hIup, "IupRadio", {P},P),
    xIupNormalizerv = define_c_func(hIup, "IupNormalizer", {P},P),
    xIupCboxv = define_c_func(hIup, "IupCboxv", {P},P),
    xIupSbox  = define_c_func(hIup, "IupSbox", {P},P),
    xIupSplit = define_c_func(hIup, "IupSplit", {P,P},P)

global function IupFill()
    return c_func(xIupFill, {})
end function

--**
-- Horizontal box
--
-- Parameters:
--   * ##children## - Sequence (or NULL) of children to place into the box
--   * ##attributes## - Optional attributes to apply after creation
--
-- Notes:
--   This method varies from IupHbox in that it allows optional attributes to be
--   assigned on creation via the ##attributes## method.
--

global function IupHboxv(object children, sequence attributes = {})
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupHboxv, {pChildren})

    if sequence(children) then
        free(pChildren)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes)
    end if

    return ih
end function

--**
-- Vertical box
--
-- Parameters:
--   * ##children## - Sequence (or NULL) of children to place into the box
--   * ##attributes## - Optional attributes to apply after creation
--
-- Notes:
--   This method varies from IupVbox in that it allows optional attributes to be
--   assigned on creation via the ##attributes## method.
--

global function IupVboxv(object children, sequence attributes = {})
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupVboxv, {pChildren})

    if sequence(children) then
        free(pChildren)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes)
    end if

    return ih
end function

--**
-- Creates a void container for composing elements in hidden layers with only one layer
-- visible at a time. It is a box that piles up the children it contains, only one child
-- is visible.
--
-- Parameters:
--   * ##children## - Sequence (or NULL) of children to place into the box
--   * ##attributes## - Optional attributes to apply after creation
--
-- Notes:
--   This method varies from IupZbox in that it allows optional attributes to be
--   assigned on creation via the ##attributes## method.
--

global function zbox(object children, sequence attributes = {})
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupZboxv, {pChildren})

    if sequence(children) then
        free(pChildren)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes)
    end if

    return ih
end function

global function IupRadio(Ihandle pChild)
    return c_func(xIupRadio, {pChild})
end function

global function normalizer(object children)
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupNormalizerv, {pChildren})

    if sequence(children) then
        free(pChildren)
    end if

    return ih
end function

global function cbox(object children)
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupCboxv, {pChildren})

    if sequence(children) then
        free(pChildren)
    end if

    return ih
end function

global function sbox(Ihandle pChild)
    return c_func(xIupSbox, {pChild})
end function

global function iup_split(Ihandle child1, Ihandle child2)
    return c_func(xIupSplit, {child1, child2})
end function

--****
-- === Layout/Hierarchy
--

constant
    xIupAppend = define_c_func(hIup, "IupAppend", {P,P},P),
    xIupDetach = define_c_proc(hIup, "IupDetach", {P}),
    xIupInsert = define_c_func(hIup, "IupInsert", {P,P,P},P),
    xIupReparent = define_c_func(hIup, "IupReparent", {P,P,P},P),
    xIupGetParent = define_c_func(hIup, "IupGetParent", {P},P),
    xIupGetChild = define_c_func(hIup, "IupGetChild", {P,I},P),
    xIupGetChildPos = define_c_func(hIup, "IupGetChildPos", {P,P},I),
    xIupGetChildCount = define_c_func(hIup, "IupGetChildCount", {P},I),
    xIupGetNextChild = define_c_func(hIup, "IupGetNextChild", {P,P},P),
    xIupGetBrother = define_c_func(hIup, "IupGetBrother", {P},P),
    xIupGetDialog = define_c_func(hIup, "IupGetDialog", {P},P),
    xIupGetDialogChild = define_c_func(hIup, "IupGetDialogChild", {P,P},P)

global function iup_append(Ihandle ih, object child)
    if atom(child) then
        if child!=NULL then
            return c_func(xIupAppend, {ih, child})
        end if
--  elsif sequence(child) then
    else
        for i=1 to length(child) do
            return c_func(xIupAppend, {ih, child[i]})
        end for
    end if
end function

global procedure iup_detach(Ihandle ih)
    c_proc(xIupDetach, {ih})
end procedure

global function iup_insert(Ihandle ih, Ihandle ref_child, Ihandle new_child)
    return c_func(xIupInsert, {ih, ref_child, new_child})
end function

global function reparent(Ihandle child, Ihandle new_parent, Ihandle ref_child)
    return c_func(xIupReparent, {child, new_parent, ref_child})
end function

global function IupGetParent(Ihandle ih)
    return c_func(xIupGetParent, {ih})
end function

global function IupGetChild(Ihandle ih, integer pos)
    return c_func(xIupGetChild, {ih, pos})
end function

global function IupGetChildPos(Ihandle ih, Ihandle child)
    return c_func(xIupGetChildPos, {ih, child})
end function

global function get_child_count(Ihandle ih)
    return c_func(xIupGetChildCount, {ih})
end function

global function get_next_child(Ihandle ih, Ihandle child)
    return c_func(xIupGetNextChild, {ih, child})
end function

global function IupGetBrother(Ihandle ih)
    return c_func(xIupGetBrother, {ih})
end function

global function IupGetDialog(Ihandle ih)
    return c_func(xIupGetDialog, {ih})
end function

global function IupGetDialogChild(Ihandle ih, sequence name)
atom pName = allocate_string(name)
Ihandle child = c_func(xIupGetDialogChild, {ih, pName})
    free(pName)
    return child
end function

--****
-- === Layout Utilities
--

constant
    xIupRefresh = define_c_proc(hIup, "IupRefresh", {P}),
    xIupRefreshChildren = define_c_proc(hIup, "IupRefreshChildren", {P}),
    xIupUpdate = define_c_proc(hIup, "IupUpdate", {P}),
    xIupUpdateChildren = define_c_proc(hIup, "IupUpdate", {P}),
    xIupRedraw = define_c_proc(hIup, "IupRedraw", {P,I}),
    xIupConvertXYToPos = define_c_func(hIup, "IupConvertXYToPos", {P,I,I},I)

global procedure IupRefresh(Ihandle ih)
    c_proc(xIupRefresh, {ih})
end procedure

global procedure refresh_children(Ihandle ih)
    c_proc(xIupRefreshChildren, {ih})
end procedure

global procedure update(Ihandle ih)
    c_proc(xIupUpdate, {ih})
end procedure

global procedure update_children(Ihandle ih)
    c_proc(xIupUpdateChildren, {ih})
end procedure

global procedure redraw(Ihandle ih, integer children)
    c_proc(xIupRedraw, {ih, children})
end procedure

global function convert_xy_to_pos(Ihandle ih, integer x, integer y)
    return c_func(xIupConvertXYToPos, {ih, x, y})
end function

--include dialog.e
constant
    xIupDialog = define_c_func(hIup, "IupDialog", {P},P),
    xIupPopup = define_c_func(hIup, "IupPopup", {P,I,I},I),
    xIupShow = define_c_func(hIup, "IupShow", {P},I),
    xIupShowXY = define_c_func(hIup, "IupShowXY", {P,I,I},I),
    xIupHide = define_c_func(hIup, "IupHide", {P},P)

--**
-- Create a dialog
--
-- Parameters:
--   * ##ih## - dialog child widget
--   * [##attributes##] - attributes to set on newly created dialog
--   * [##data##] - data to sent to sprintf() type formatting for ##attributes##
--
-- Notes:
--   This method differs from ##IupDialog## in that you can pass attributes to be applied
--   to the dialog on creation.
--
-- See Also:
--   [[::show]], [[:show_xy]]
--

global function IupDialog(Ihandle ih, sequence attributes = {}, sequence data = {})
Ihandle dlg = c_func(xIupDialog, {ih})

    if length(attributes) then
        IupSetAttributes(dlg, attributes, data)
    end if

    return dlg
end function

global function IupPopup(Ihandle ih, integer x, integer y)
    return c_func(xIupPopup, {ih, x, y})
end function

global function IupShow(Ihandle ih)
    return c_func(xIupShow, {ih})
end function

global function IupShowXY(Ihandle ih, integer x, integer y)
    return c_func(xIupShowXY, {ih, x, y})
end function

global function IupHide(Ihandle ih)
    return c_func(xIupHide, {ih})
end function

--****
-- === Predefined
--

constant
    xIupFileDlg = define_c_func(hIup, "IupFileDlg", {},P),
    xIupMessageDlg = define_c_func(hIup, "IupMessageDlg", {},P),
    xIupColorDlg = define_c_func(hIup, "IupColorDlg", {},P),
    xIupFontDlg = define_c_func(hIup, "IupFontDlg", {},P),
    xIupAlarm = define_c_func(hIup, "IupAlarm", {P,P,P,P,P},I),
    xIupGetFile = define_c_func(hIup, "IupGetFile", {P},I),
    xIupGetColor = define_c_func(hIup, "IupGetColor", {I,I,P,P,P},I),
    xIupGetParam = define_c_func(hIup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupGetText = define_c_func(hIup, "IupGetText", {P,P},P),
    xIupListDialog = define_c_func(hIup, "IupListDialog", {I,P,I,P,I,I,I,P},I),
    xIupMessage  = define_c_proc(hIup, "IupMessage", {P,P}),
    xIupLayoutDialog = define_c_func(hIup, "IupLayoutDialog", {P},P)

global function IupFileDlg()
    return c_func(xIupFileDlg, {})
end function

global function message_dlg()
    return c_func(xIupMessageDlg, {})
end function

global function color_dlg()
    return c_func(xIupColorDlg, {})
end function

global function IupFontDlg()
    return c_func(xIupFontDlg, {})
end function

global function IupAlarm(sequence title, sequence message, object b1 = 0, object b2 = 0, object b3 = 0)
atom pB1 = NULL, pB2 = NULL, pB3 = NULL

atom pTitle = allocate_string(title)
atom pMessage = allocate_string(message)

    if sequence(b1) then
        pB1 = allocate_string(b1)
    end if
    if sequence(b2) then
        pB2 = allocate_string(b2)
    end if
    if sequence(b3) then
        pB3 = allocate_string(b3)
    end if

    integer r = c_func(xIupAlarm, {pTitle, pMessage, pB1, pB2, pB3})

    free(pTitle)
    free(pMessage)
    free(pB1) -- free ignores (0)
    free(pB2)
    free(pB3)

    return r
end function

global function get_file(sequence filename)
atom pFilename = allocate(1024)
    poke(pFilename, filename & 0)

    if c_func(xIupGetFile, {pFilename})= -1 then
        free(pFilename)
        return NULL
    end if

    filename = peek_string(pFilename)
    free(pFilename)

    return filename
end function

global function IupGetColor(integer x=IUP_CENTERPARENT, integer y=IUP_CENTERPARENT,integer r=255, integer g=255, integer b=255)
atom pR, pG, pB

    pR = allocate(1)
    poke(pR, r)

    pG = allocate(1)
    poke(pG, g)

    pB = allocate(1)
    poke(pB, b)

    integer result = c_func(xIupGetColor, {x, y, pR, pG, pB})

    r = peek(pR)
    g = peek(pG)
    b = peek(pB)

    free(pR)
    free(pG)
    free(pB)

    return {result, r, g, b}
end function

function get_param_set(object param, integer len = 1024)
atom p
    if sequence(param) then
        p = allocate(len*4)
        mem_set(p, 0, len)
        poke(p, param)
    elsif integer(param) then
        p = allocate(4)
        poke4(p, param)
    else
        p = allocate(8)
        poke_double(p, param)
    end if

    return p
end function

global function get_param_get(object param, atom p)
    if sequence(param) then
        return peek_string(p)
    elsif integer(param) then
        return peek4s(p)
    else
        return peek_double(p)
    end if
end function

global function IupGetParam(sequence title, integer rid, object data, sequence fmt, sequence args)
atom pTitle = allocate_string(title)
atom pRid = 0
atom pData = data
atom pFormat = allocate_string(fmt)
atom p1 = 0, p2 = 0, p3 = 0, p4 = 0, p5 = 0
atom p6 = 0, p7 = 0, p8 = 0, p9 = 0, p10 = 0
integer la

    if rid>=0 then
        pRid = call_back({'+', rid})
    end if

--DEV...
--  switch length(args) with fallthru do
--      case 10 then
--          p10 = get_param_set(args[10])
--      case 9 then
--          p9 = get_param_set(args[9])
--      case 8 then
--          p8 = get_param_set(args[8])
--      case 7 then
--          p7 = get_param_set(args[7])
--      case 6 then
--          p6 = get_param_set(args[6])
--      case 5 then
--          p5 = get_param_set(args[5])
--      case 4 then
--          p4 = get_param_set(args[4])
--      case 3 then
--          p3 = get_param_set(args[3])
--      case 2 then
--          p2 = get_param_set(args[2])
--      case 1 then
--          p1 = get_param_set(args[1])
--  end switch
    la = length(args)
    if la>=10 then
        p10 = get_param_set(args[10])
    end if
    if la>=9 then
        p9 = get_param_set(args[9])
    end if
    if la>=8 then
        p8 = get_param_set(args[8])
    end if
    if la>=7 then
        p7 = get_param_set(args[7])
    end if
    if la>=6 then
        p6 = get_param_set(args[6])
    end if
    if la>=5 then
        p5 = get_param_set(args[5])
    end if
    if la>=4 then
        p4 = get_param_set(args[4])
    end if
    if la>=3 then
        p3 = get_param_set(args[3])
    end if
    if la>=2 then
        p2 = get_param_set(args[2])
    end if
    if la>=1 then
        p1 = get_param_set(args[1])
    end if

    integer result = c_func(xIupGetParam, {pTitle, pRid, pData, pFormat,
                                           p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 0})

    sequence vals = {}

--  switch length(args) with fallthru do
--      case 10 then
--          vals = prepend(vals, get_param_get(args[10], p10))
--          free(p10)
--          
--      case 9 then
--          vals = prepend(vals, get_param_get(args[9], p9))
--          free(p9)
--          
--      case 8 then
--          vals = prepend(vals, get_param_get(args[8], p8))
--          free(p8)
--          
--      case 7 then
--          vals = prepend(vals, get_param_get(args[7], p7))
--          free(p7)
--          
--      case 6 then
--          vals = prepend(vals, get_param_get(args[6], p6))
--          free(p6)
--          
--      case 5 then
--          vals = prepend(vals, get_param_get(args[5], p5))
--          free(p5)
--          
--      case 4 then
--          vals = prepend(vals, get_param_get(args[4], p4))
--          free(p4)
--          
--      case 3 then
--          vals = prepend(vals, get_param_get(args[3], p3))
--          free(p3)
--          
--      case 2 then
--          vals = prepend(vals, get_param_get(args[2], p2))
--          free(p2)
--          
--      case 1 then
--          vals = prepend(vals, get_param_get(args[1], p1))
--          free(p1)
--  end switch
    if la>=10 then
        vals = prepend(vals, get_param_get(args[10], p10))
        free(p10)
    end if
    if la>=9 then
        vals = prepend(vals, get_param_get(args[9], p9))
        free(p9)
    end if
    if la>=8 then
        vals = prepend(vals, get_param_get(args[8], p8))
        free(p8)
    end if
    if la>=7 then
        vals = prepend(vals, get_param_get(args[7], p7))
        free(p7)
    end if
    if la>=6 then
        vals = prepend(vals, get_param_get(args[6], p6))
        free(p6)
    end if
    if la>=5 then
        vals = prepend(vals, get_param_get(args[5], p5))
        free(p5)
    end if
    if la>=4 then
        vals = prepend(vals, get_param_get(args[4], p4))
        free(p4)
    end if
    if la>=3 then
        vals = prepend(vals, get_param_get(args[3], p3))
        free(p3)
    end if
    if la>=2 then
        vals = prepend(vals, get_param_get(args[2], p2))
        free(p2)
    end if
    if la>=1 then
        vals = prepend(vals, get_param_get(args[1], p1))
        free(p1)
    end if

    return vals & result
end function

global function get_dialog_text(sequence title, sequence value, integer max_size = 1024)
sequence text = ""
atom pTitle = allocate_string(title)
atom pValue = allocate(max_size)

    mem_set(pValue, 0, max_size)
    poke(pValue, value)

    if c_func(xIupGetText, {pTitle, pValue})!=0 then
        text = peek_string(pValue)
    end if

    free(pTitle)
    free(pValue)

    return text
end function

--**
-- Shows a modal dialog to select items from a simple or multiple selection list.
-- 
-- Parameters:
--   * ##typ## - 1, simple selection, single integer result. 2, multi-selection, 
--               sequence of integers result.
--   * ##title##
--   * ##options##
--   * ##selected##
--   * ##maxColumns##
--   * ##maxLines##
--

global function list_dialog(integer typ, sequence title, sequence options, integer selected,
        integer maxColumns, integer maxLines)
integer result, size
atom pTitle, pOptions, pMark
sequence pOptAry, marked
object fnVal

    --pOptions = allocate_string_pointers_array(options)
    size = length(options)
    pOptions = allocate(4*size)
    pMark = allocate(4*size)
    mem_set(pMark,0,4*size)
    pOptAry = {}
    for i=1 to size do
        pOptAry &= allocate_string(options[i])
    end for
    poke4(pOptions, pOptAry)

    pTitle = allocate_string(title)
    result = c_func(xIupListDialog, {
                                     typ, pTitle, size, pOptions, selected, maxColumns, maxLines, pMark
                                    })
    free(pTitle)

    for i=1 to length(options) do
        free(pOptAry[i])
    end for
    free(pOptions)

    if typ=2 then
        marked = peek4u({pMark,size})
        fnVal = {}
        for i=1 to size do
            if marked[i] then
                fnVal &= i
            end if
        end for
    else
        fnVal = result+1 -- Make it a 1 based array like Euphoria
    end if
    free(pMark)

    return fnVal
end function

global procedure IupMessage(sequence title, sequence message)
atom pTitle = allocate_string(title)
atom aMessage = allocate_string(message)
    c_proc(xIupMessage, {pTitle, aMessage})
    free(pTitle)
    free(aMessage)
end procedure

global function layout_dialog(Ihandle ih)
    return c_func(xIupLayoutDialog, {ih})
end function

--include controls.e
constant
    xIupButton = define_c_func(hIup, "IupButton", {P,P},P),
    xIupCanvas = define_c_func(hIup, "IupCanvas", {P},P),
    xIupFrame = define_c_func(hIup, "IupFrame", {P},P),
    xIupLabel = define_c_func(hIup, "IupLabel", {P},P),
    xIupList = define_c_func(hIup, "IupList", {P},P),
    xIupProgressBar = define_c_func(hIup, "IupProgressBar", {},P),
    xIupSpin = define_c_func(hIup, "IupSpin", {},P),
    xIupTabs = define_c_func(hIup, "IupTabsv", {P},P),
    xIupText = define_c_func(hIup, "IupText", {P},P),
    xIupToggle = define_c_func(hIup, "IupToggle", {P,P},P),
    xIupTree = define_c_func(hIup, "IupTree", {},P),
    xIupVal = define_c_func(hIup, "IupVal", {P},P),
    xIupTextConvertPosToLinCol = define_c_proc(hIup, "IupTextConvertPosToLinCol", {P,I,P,P}),
    xIupTextConvertLinColToPos = define_c_proc(hIup, "IupTextConvertLinColToPos", {P,I,I,P})

--**
-- Create a new button widget
--
-- Parameters:
--   * ##title## - Title for the button
--   * ##action## - Optional action name
--   * ##rid## - Optional routine id to call upon pressing the button
--
-- Notes:
--   This method varies from IupButton in that it will accept an optional
--   callback method via ##rid##. This ##rid## is assigned to the
--   ##[[:ACTION]]## callback for the newly created button widget.
--

global function IupButton(sequence title, sequence action = {}, integer rid = -1,
            sequence attributes = {}, sequence data = {})
atom pTitle = allocate_string(title)
atom pAction = 0

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupButton, {pTitle, pAction})

    free(pTitle)
    if length(action) then
        free(pAction)
    end if

    if rid>=0 then
        IupSetCallback(ih, ACTION, rid)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global function IupCanvas(sequence action = {})
atom pAction = 0

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupCanvas, {pAction})

    if pAction!=0 then
        free(pAction)
    end if

    return ih
end function

global function IupFrame(Ihandle child, sequence attributes = {}, sequence data = {})
Ihandle ih = c_func(xIupFrame, {child})

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global function IupLabel(sequence title)
atom pTitle = allocate_string(title)

Ihandle ih = c_func(xIupLabel, {pTitle})

    free(pTitle)

    return ih
end function

global function IupList(sequence action = {})
atom pAction = 0

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupList, {pAction})

    if length(action) then
        free(pAction)
    end if

    return ih
end function

global function IupProgressBar()
    return c_func(xIupProgressBar, {})
end function

global function spin()
    return c_func(xIupSpin, {})
end function

global function IupTabs(object children)
atom pChildren = 0

    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if

    Ihandle ih = c_func(xIupTabs, {pChildren})

    if sequence(children) then
--      free_pointer_array(pChildren)
        free(pChildren)
    end if

    return ih
end function

--void IupTextConvertPosToLinCol(Ihandle* ih, int pos, int *lin, int *col);
global function IupTextConvertPosToLinCol(atom ih, atom pos)
atom pLineCol = allocate(16,1)
    c_proc(xIupTextConvertPosToLinCol, {ih,pos,pLineCol,pLineCol+4})
    return peek4s({pLineCol,2})
end function

--void IupTextConvertLinColToPos(Ihandle* ih, int lin, int col, int *pos);
global function IupTextConvertLinColToPos(atom ih, atom lin, atom col)
atom pPos = allocate(4,1)
    c_proc(xIupTextConvertLinColToPos, {ih,lin,col,pPos})
    return peek4s(pPos)
end function


--**
-- Create a text widget
--
-- Parameters:
--   * [##action##] - Action name to associate with text widget
--   * [##attributes##] - Attributes to apply to newly created text widget
--   * [##data##] - Data to pass on to sprintf() type formatting of ##attributes##
--

global function IupText(sequence action = {}, sequence attributes = {}, sequence data = {})
atom pAction = 0

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupText, {pAction})

    if length(action) then
        free(pAction)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global function IupToggle(sequence title, sequence action = {})
atom pTitle = allocate_string(title)
atom pAction = 0

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupToggle, {pTitle, pAction})

    free(pTitle)
    if length(action) then
        free(pAction)
    end if

    return ih
end function

global function IupTree()
    return c_func(xIupTree, {})
end function

global function IupVal(object orientation=NULL)
atom pOrientation = NULL
    if string(orientation) then
        pOrientation = allocate_string(orientation)
    end if
    Ihandle ih = c_func(xIupVal, {pOrientation})
    if string(orientation) then
        free(pOrientation)
    end if
    return ih
end function

global constant -- function delcarations
        xIupConfig                        = define_c_func(hIup, "IupConfig", {}, P),
        xIupConfigLoad                    = define_c_func(hIup, "IupConfigLoad", {P}, I),
        xIupConfigSave                    = define_c_func(hIup, "IupConfigSave", {P}, I),
        xIupConfigSetVariableStr          = define_c_proc(hIup, "IupConfigSetVariableStr", {P,P,P,P}),
        xIupConfigSetVariableStrId        = define_c_proc(hIup, "IupConfigSetVariableStrId", {P,P,P,I,P}),
        xIupConfigSetVariableInt          = define_c_proc(hIup, "IupConfigSetVariableInt", {P,P,P,I}),
        xIupConfigSetVariableIntId        = define_c_proc(hIup, "IupConfigSetVariableIntId", {P,P,P,I,I}),
        xIupConfigSetVariableDouble       = define_c_proc(hIup, "IupConfigSetVariableDouble", {P,P,P,D}),
        xIupConfigSetVariableDoubleId     = define_c_proc(hIup, "IupConfigSetVariableDoubleId", {P,P,P,I,D}),
        xIupConfigGetVariableStr          = define_c_func(hIup, "IupConfigGetVariableStr", {P,P,P}, P),
        xIupConfigGetVariableStrId        = define_c_func(hIup, "IupConfigGetVariableStrId", {P,P,P,I}, P),
        xIupConfigGetVariableInt          = define_c_func(hIup, "IupConfigGetVariableInt", {P,P,P}, I),
        xIupConfigGetVariableIntId        = define_c_func(hIup, "IupConfigGetVariableIntId", {P,P,P,I}, I),
        xIupConfigGetVariableDouble       = define_c_func(hIup, "IupConfigGetVariableDouble", {P,P,P}, D),
        xIupConfigGetVariableDoubleId     = define_c_func(hIup, "IupConfigGetVariableDoubleId", {P,P,P,I}, D),
        xIupConfigGetVariableStrDef       = define_c_func(hIup, "IupConfigGetVariableStrDef", {P,P,P,P}, P),
        xIupConfigGetVariableStrIdDef     = define_c_func(hIup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P),
        xIupConfigGetVariableIntDef       = define_c_func(hIup, "IupConfigGetVariableIntDef", {P,P,P,I}, I),
        xIupConfigGetVariableIntIdDef     = define_c_func(hIup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I),
        xIupConfigGetVariableDoubleDef    = define_c_func(hIup, "IupConfigGetVariableDoubleDef", {P,P,P,D}, D),
        xIupConfigGetVariableDoubleIdDef  = define_c_func(hIup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D),
        xIupConfigRecentInit              = define_c_proc(hIup, "IupConfigRecentInit", {P,P,P,I}),
        xIupConfigRecentUpdate            = define_c_proc(hIup, "IupConfigRecentUpdate", {P,P}),
        xIupConfigDialogShow              = define_c_proc(hIup, "IupConfigDialogShow", {P,P,P}),
        xIupConfigDialogClosed            = define_c_proc(hIup, "IupConfigDialogClosed", {P,P,P}),
$

--Ihandle* IupConfig(void);
public function IupConfig()
atom result = c_func(xIupConfig, {})
    return result
end function

--int IupConfigLoad(Ihandle* ih);
public function IupConfigLoad(atom ih)
atom result = c_func(xIupConfigLoad, {ih})
    return result
end function

--int IupConfigSave(Ihandle* ih);
public function IupConfigSave(atom ih)
atom result = c_func(xIupConfigSave, {ih})
    return result
end function

/****************************************************************/
--void IupConfigSetVariableStr(Ihandle* ih, const char* group, const char* key, const char* v);
public procedure IupConfigSetVariableStr(atom ih, object group = NULL, object key = NULL, object v = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(v) then v = allocate_string(v,1) end if
    c_proc(xIupConfigSetVariableStr, {ih,group,key,v})
end procedure

--void IupConfigSetVariableStrId(Ihandle* ih, const char* group, const char* key, int id, const char* v);
public procedure IupConfigSetVariableStrId(atom ih, object group, object key, atom id, object v = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(v) then v = allocate_string(v,1) end if
    c_proc(xIupConfigSetVariableStrId, {ih,group,key,id,v})
end procedure

--void IupConfigSetVariableInt(Ihandle* ih, const char* group, const char* key, int v);
public procedure IupConfigSetVariableInt(atom ih, object group, object key, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableInt, {ih,group,key,v})
end procedure

--void IupConfigSetVariableIntId(Ihandle* ih, const char* group, const char* key, int id, int v);
public procedure IupConfigSetVariableIntId(atom ih, object group, object key, atom id, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableIntId, {ih,group,key,id,v})
end procedure

--void IupConfigSetVariableDouble(Ihandle* ih, const char* group, const char* key, double v);
public procedure IupConfigSetVariableDouble(atom ih, object group, object key, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableDouble, {ih,group,key,v})
end procedure

--void IupConfigSetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id, double v);
public procedure IupConfigSetVariableDoubleId(atom ih, object group, object key, atom id, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableDoubleId, {ih,group,key,id,v})
end procedure

--const char* IupConfigGetVariableStr(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableStr(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableStr, {ih,group,key})
--DEV apply to tee(*2) [and all the ,1]:
    return peek_string(result)
end function

--const char* IupConfigGetVariableStrId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableStrId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableStrId, {ih,group,key,id})
    return peek_string(result)
end function

--int IupConfigGetVariableInt(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableInt(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableInt, {ih,group,key})
    return result
end function

--int IupConfigGetVariableIntId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableIntId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntId, {ih,group,key,id})
    return result
end function

--double IupConfigGetVariableDouble(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableDouble(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDouble, {ih,group,key})
    return result
end function

--double IupConfigGetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableDoubleId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleId, {ih,group,key,id})
    return result
end function

--const char* IupConfigGetVariableStrDef(Ihandle* ih, const char* group, const char* key, const char* def);
public function IupConfigGetVariableStrDef(atom ih, object group = NULL, object key = NULL, object def = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(def) then def = allocate_string(def,1) end if
    atom result = c_func(xIupConfigGetVariableStrDef, {ih,group,key,def})
    return result
end function

--const char* IupConfigGetVariableStrIdDef(Ihandle* ih, const char* group, const char* key, int id, const char* def);
public function IupConfigGetVariableStrIdDef(atom ih, object group, object key, atom id, object def = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(def) then def = allocate_string(def,1) end if
    atom result = c_func(xIupConfigGetVariableStrIdDef, {ih,group,key,id,def})
    return result
end function

--int IupConfigGetVariableIntDef(Ihandle* ih, const char* group, const char* key, int def);
public function IupConfigGetVariableIntDef(atom ih, object group, object key, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntDef, {ih,group,key,def})
    return result
end function

--int IupConfigGetVariableIntIdDef(Ihandle* ih, const char* group, const char* key, int id, int def);
public function IupConfigGetVariableIntIdDef(atom ih, object group, object key, atom id, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntIdDef, {ih,group,key,id,def})
    return result
end function

--double IupConfigGetVariableDoubleDef(Ihandle* ih, const char* group, const char* key, double def);
public function IupConfigGetVariableDoubleDef(atom ih, object group, object key, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleDef, {ih,group,key,def})
    return result
end function

--double IupConfigGetVariableDoubleIdDef(Ihandle* ih, const char* group, const char* key, int id, double def);
public function IupConfigGetVariableDoubleIdDef(atom ih, object group, object key, atom id, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleIdDef, {ih,group,key,id,def})
    return result
end function

/****************************************************************/
--void IupConfigRecentInit(Ihandle* ih, Ihandle* menu, Icallback recent_cb, int max_recent);
public procedure IupConfigRecentInit(atom ih, atom menu, atom recent_cb, atom max_recent)
--DEV _cb??
    c_proc(xIupConfigRecentInit, {ih,menu,recent_cb,max_recent})
end procedure

--void IupConfigRecentUpdate(Ihandle* ih, const char* filename);
public procedure IupConfigRecentUpdate(atom ih, object filename = NULL)
    if sequence(filename) then filename = allocate_string(filename,1) end if
    c_proc(xIupConfigRecentUpdate, {ih,filename})
end procedure

--void IupConfigDialogShow(Ihandle* ih, Ihandle* dialog, const char* name);
public procedure IupConfigDialogShow(atom ih, atom dialog, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupConfigDialogShow, {ih,dialog,name})
end procedure

--void IupConfigDialogClosed(Ihandle* ih, Ihandle* dialog, const char* name);
public procedure IupConfigDialogClosed(atom ih, atom dialog, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupConfigDialogClosed, {ih,dialog,name})
end procedure


--****
-- === Additional
--

constant hIupControls = iup_open_dll({
                                      "iupcontrols.dll",
                                      "libiupcontrols.so",
                                      "libiupcontrols.dylib"
                                     })

constant
    xIupControlsOpen = define_c_func(hIupControls, "IupControlsOpen", {},I),
    xIupCells = define_c_func(hIupControls, "IupCells", {},P),
    xIupColorbar = define_c_func(hIupControls, "IupColorbar", {},P),
    xIupColorBrowser = define_c_func(hIupControls, "IupColorBrowser", {},P),
    xIupDial = define_c_func(hIupControls, "IupDial", {P},P),
    xIupMatrix = define_c_func(hIupControls, "IupMatrix", {P},P),
    xIupMatSetAttribute = define_c_proc(hIupControls, "IupMatSetAttribute",{P,P,I,I,P}),
    xIupMatStoreAttribute = define_c_proc(hIupControls, "IupMatStoreAttribute",{P,P,I,I,P}),
    xIupMatGetAttribute = define_c_func(hIupControls, "IupMatrixGetAttribute",{P,P,I,I},P),
    xIupMatGetInt = define_c_func(hIupControls, "IupMatrixGetInt",{P,P,I,I},I),
    xIupMatGetFloat = define_c_func(hIupControls, "IupMatrixGetFloat",{P,P,I,I},F)

integer did_iup_controls_open = 0

function controls_open()
    did_iup_controls_open = 1

    return c_func(xIupControlsOpen, {})
end function

global function cells()
    if not did_iup_controls_open then
        {} = controls_open()
    end if

    return c_func(xIupCells, {})
end function

global function colorbar()
    if not did_iup_controls_open then
        {} = controls_open()
    end if

    return c_func(xIupColorbar, {})
end function

global function color_browser()
    if not did_iup_controls_open then
        {} = controls_open()
    end if

    return c_func(xIupColorBrowser, {})
end function

global function dial(sequence orientation)
    if not did_iup_controls_open then
        {} = controls_open()
    end if

    atom pOrientation = allocate_string(orientation)
    Ihandle ih = c_func(xIupDial, {pOrientation})
    free(pOrientation)
    return ih
end function

global function matrix(object action = 0, sequence attributes = {}, sequence data = {})
    if not did_iup_controls_open then
        {} = controls_open()
    end if

    atom pAction = 0
    if sequence(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupMatrix, {pAction})

    free(pAction)

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global procedure mat_set_attribute(Ihandle ih, sequence name, integer lin, integer col, atom pValue)
atom pName = allocate_string(name)
    c_proc(xIupMatSetAttribute, {ih, pName, lin, col, pValue})
    free(pName)
end procedure

global procedure mat_store_attribute(Ihandle ih, sequence name, integer lin, integer col, sequence val, sequence data = {})
    if length(data) then
        val = sprintf(val, data)
    end if

    atom pName = allocate_string(name)
    atom pValue = allocate_string(val)

    c_proc(xIupMatStoreAttribute, {ih, pName, lin, col, pValue})

    free(pName)
    free(pValue)
end procedure

global function mat_get_attribute(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)
atom pValue = c_func(xIupMatGetAttribute, {ih, pName, lin, col})

    free(pName)

    return peek_string(pValue)
end function

global function mat_get_int(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)

integer val = c_func(xIupMatGetInt, {ih, pName, lin, col})

    free(pName)

    return val
end function

global function mat_get_float(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)

atom val = c_func(xIupMatGetFloat, {ih, pName, lin, col})

    free(pName)

    return val
end function

--****
-- === Additional Web
--

constant hIupWeb = iup_open_dll({
                                 "iupweb.dll",
                                 "libiupweb.so",
                                 "libiupweb.dylib"
                                })

constant
    xIupWebBrowserOpen = define_c_func(hIupWeb, "IupWebBrowserOpen", {},I),
    xIupWebBrowser = define_c_func(hIupWeb, "IupWebBrowser", {},P)

integer did_web_browser_open = 0

function web_browser_open()
    did_web_browser_open = 1

    return c_func(xIupWebBrowserOpen, {})
end function

global function web_browser()
    if not did_web_browser_open then
        {} = web_browser_open()
    end if

    return c_func(xIupWebBrowser, {})
end function

--include resources.e

--****
-- === Images
--

constant hIupIm = iup_open_dll({
                                "iupim.dll",
                                "libiupim.so",
                                "libiupim.dylib"
                               })

constant
    xIupImage = define_c_func(hIup, "IupImage", {I,I,P},P),
    xIupImageRGB = define_c_func(hIup, "IupImageRGB", {I,I,P},P),
    xIupImageRGBA = define_c_func(hIup, "IupImageRGBA", {I,I,P},P),
    xIupLoadImage = define_c_func(hIupIm, "IupLoadImage", {P},P),
    xIupSaveImage = define_c_func(hIupIm, "IupSaveImage", {P,P,P},I),
    xIupGetNativeHandleImage = define_c_func(hIupIm, "IupGetNativeHandleImage", {P},P),
    xIupGetImageNativeHandle = define_c_func(hIupIm, "IupGetImageNativeHandle", {P},P),
    xIupSaveImageAsText = define_c_func(hIup, "IupSaveImageAsText", {P,P,P,P},I)

global function IupImage(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageA(integer width, integer height, atom pPixels)
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    return ih
end function

global function image_rgb(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImageRGB, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageRGBA(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImageRGBA, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupLoadImage(sequence filename)
atom pFilename = allocate_string(filename)
Ihandle ih = c_func(xIupLoadImage, {pFilename})
    free(pFilename)
    return ih
end function

global function save_image(Ihandle ih, sequence filename, sequence format)
atom pFilename = allocate_string(filename)
atom pFormat = allocate_string(format)
integer result = c_func(xIupSaveImage, {ih, pFilename, pFormat})
    free(pFilename)
    free(pFormat)
    return result
end function

global function save_image_as_text(Ihandle ih, sequence filename, sequence format, sequence name)
atom pFilename = allocate_string(filename)
atom pFormat = allocate_string(format)
atom pName = allocate_string(name)
integer result = c_func(xIupSaveImageAsText, {ih, pFilename, pFormat, pName})
    free(pFilename)
    free(pFormat)
    free(pName)
    return result
end function

global function get_native_handle_image(Ihandle ih)
    return c_func(xIupGetNativeHandleImage, {ih})
end function

global function get_image_native_handle(atom handle)
    return c_func(xIupGetImageNativeHandle, {handle})
end function

--****
-- === Keyboard
--

constant
    xIupNextField = define_c_func(hIup, "IupNextField", {P},P),
    xIupPreviousField = define_c_func(hIup, "IupPreviousField", {P},P),
    xIupGetFocus = define_c_func(hIup, "IupGetFocus", {},P),
    xIupSetFocus = define_c_func(hIup, "IupSetFocus", {P},P)

global function IupNextField(atom h)
    return c_func(xIupNextField, {h})
end function

global function IupPreviousField(atom h)
    return c_func(xIupPreviousField, {h})
end function

global function IupGetFocus()
    return c_func(xIupGetFocus, {})
end function

global function IupSetFocus(atom h)
    return c_func(xIupSetFocus, {h})
end function

--****
-- === Menus
--

constant
    xIupItem = define_c_func(hIup, "IupItem",  {P,P},P),
    xIupMenuv = define_c_func(hIup, "IupMenuv", {P},P),
    xIupSeparator = define_c_func(hIup, "IupSeparator", {},P),
    xIupSubmenu = define_c_func(hIup, "IupSubmenu", {P,P},P)

global function IupItem(sequence title, object action = {}, integer rid = -2)
atom pTitle = allocate_string(title)
atom pAction = 0

    if sequence(action) and length(action)>0 then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupItem, {pTitle, pAction})

    free(pTitle)
    if sequence(action) and length(action)>0 then
        free(pAction)
    end if

    if rid>=0 then
        IupSetCallback(ih, ACTION, rid)
    end if

    return ih
end function

global function IupMenuv(sequence children)
atom pChildren = allocate_pointer_array(children)
Ihandle ih = c_func(xIupMenuv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupSeparator()
    return c_func(xIupSeparator, {})
end function

global function IupSubmenu(sequence title, Ihandle menu)
atom pTitle = allocate_string(title)
Ihandle ih = c_func(xIupSubmenu, {pTitle, menu})
    free(pTitle)
    return ih
end function

--****
-- === Names
--

constant
    xIupSetHandle = define_c_func(hIup, "IupSetHandle", {P,P},P),
    xIupGetHandle = define_c_func(hIup, "IupGetHandle", {P},P),
    xIupGetName = define_c_func(hIup, "IupGetName", {P},P),
    xIupGetAllNames = define_c_func(hIup, "IupGetAllNames", {P,I},I),
    xIupGetAllDialogs = define_c_func(hIup, "IupGetAllDialogs", {P,I},I)

global function IupSetHandle(sequence name, Ihandle ih)
atom pName = allocate_string(name)
    ih = c_func(xIupSetHandle, {pName, ih})
    free(pName)
    return ih
end function

global function get_handle(sequence name)
atom pName = allocate_string(name)
Ihandle ih = c_func(xIupGetHandle, {pName})
    free(pName)
    return ih
end function

global function get_name(Ihandle ih)
atom pName = c_func(xIupGetName, {ih})
    if not pName then
        return ""
    end if

    return peek_string(pName)
end function

--**
-- Returns the names of all interface elements that have an associated name using 
-- IupSetHandle or using LED.
-- 

global function get_all_names(integer max_n = 128)
    return get_string_array(xIupGetAllNames, max_n)
end function

global function get_all_dialogs(integer max_n = 128)
    return get_string_array(xIupGetAllDialogs, max_n)
end function

--include mouse.e
global function isshift(atom pchar)
    return peek(pchar)='S'
end function

global function iscontrol(atom pchar)
    return peek(pchar+1)='C'
end function

global function isbutton1(atom pchar)
    return peek(pchar+2)='1'
end function

global function isbutton2(atom pchar)
    return peek(pchar+3)='2'
end function

global function isbutton3(atom pchar)
    return peek(pchar+4)='3'
end function

global function isbutton4(atom pchar)
    return peek(pchar+8)='4'
end function

global function isbutton5(atom pchar)
    return peek(pchar+9)='5'
end function

global function isdouble(atom pchar)
    return peek(pchar+5)='D'
end function

global function isalt(atom pchar)
    return peek(pchar+6)='A'
end function

global function issys(atom pchar)
    return peek(pchar+7)='Y'
end function

--include iupmisc.e
constant
    xIupClipboard = define_c_func(hIup, "IupClipboard", {},P),
    xIupTimer = define_c_func(hIup, "IupTimer", {},P),
    xIupUser = define_c_func(hIup, "IupUser", {},P),
    xIupHelp = define_c_func(hIup, "IupHelp", {P},I)

global function IupClipboard()
    return c_func(xIupClipboard, {})
end function

global function IupTimer(integer rid = -2, integer msecs = 0, integer active = 1)
Ihandle ih = c_func(xIupTimer, {})

    if rid>=0 and msecs!=0 then
        IupSetCallback(ih, ACTION_CB, rid)
        IupSetAttributes(ih, "TIME=%d,RUN=%s", {
                                                msecs, iff(active, "YES", "NO")
                                               })
    end if

    return ih
end function

global function user()
    return c_func(xIupUser, {})
end function

global function IupHelp(sequence url)
atom pUrl = allocate_string(url)

integer result = c_func(xIupHelp, {pUrl})

    free(pUrl)

    return result
end function

--cd.e:
--constant
atom
    hCd = iup_open_dll({
                        "cd.dll",
                        "libcd.so",
                        "libcd.dylib"
                       }),
    hCdIup = iup_open_dll({
                           "iupcd.dll",
                           "libiupcd.so",
                           "libiupcd.dylib"
                          })

--****
-- === Canvas Draw Constants

global constant
    CD_QUERY = -1,
    CD_ERROR = -1,
    CD_OK = 0

--****
-- === Polygon Mode Constants

global constant
    CD_FILL = 0,
    CD_OPEN_LINES = 1,
    CD_CLOSED_LINES = 2,
    CD_CLIP = 3,
    CD_BEZIER = 4,
    CD_REGION = 5,
    CD_POLYCUSTOM = 10

--****
-- === Bitmap Type Constants
--
--  These definitions are compatible with the IM library

global constant
    CD_RGB = 0,
    CD_MAP = 1,
    CD_RGBA = #100

--****
-- === Bitmap Data Constants

global constant
    CD_IRED = 0,
    CD_IGREEN = 1,
    CD_IBLUE = 2,
    CD_IALPHA = 3,
    CD_INDEX = 4,
    CD_COLORS = 5

--****
-- === Clip Mode Constants
--

global constant
    CD_CLIPOFF = 0,
    CD_CLIPAREA = 1,
    CD_CLIPPOLYGON = 2,
    CD_CLIPREGION = 3

--****
-- === Region Combine Mode Constants
--

global constant
    CD_UNION = 0,
    CD_INTERSECT = 1,
    CD_DIFFERENCE = 2,
    CD_NOTINTERSECT = 3

--****
-- === Fill Mode Constants
--

global constant
    CD_EVENODD = 0,
    CD_WINDING = 1

--****
-- === Line Join Constants
--

global constant
    CD_MITER = 0,
    CD_BEVEL = 1,
    CD_ROUND = 2

--****
-- === Line Cap Constants
--

global constant
    CD_CAPFLAT = 0,
    CD_CAPSQUARE = 1,
    CD_CAPROUND = 2

--****
-- === Background Opacity Mode Constants
--

global constant
    CD_OPAQUE = 0,
    CD_TRANSPARENT = 1

--****
-- === Write Mode Constants
--

global constant
    CD_REPLACE = 0,
    CD_XOR = 1,
    CD_NOT_XOR = 2

--****
-- === Color Allocation Mode Constants
--
-- Pallette
--

global constant
    CD_POLITE = 0,
    CD_FORCE = 1

--****
-- === Line Style Constants
--

global constant
    CD_CONTINUOUS = 0,
    CD_DASHED = 1,
    CD_DOTTED = 2,
    CD_DASH_DOT = 3,
    CD_DASH_DOT_DOT = 4,
    CD_CUSTOM = 5

--****
-- === Marker Type Constants
--

global constant
    CD_PLUS = 0,
    CD_STAR = 1,
    CD_CIRCLE = 2,
    CD_X = 3,
    CD_BOX = 4,
    CD_DIAMOND = 5,
    CD_HOLLOW_CIRCLE = 6,
    CD_HOLLOW_BOX = 7,
    CD_HOLLOW_DIAMOND = 8

--****
-- === Hatch Type Constants
--

global constant
    CD_HORIZONTAL = 0,
    CD_VERTICAL = 1,
    CD_FDIAGONAL = 2,
    CD_BDIAGONAL = 3,
    CD_CROSS = 4,
    CD_DIAGCROSS = 5

--****
-- === Interior Style Constants
--

global constant
    CD_SOLID = 0,
    CD_HATCH = 1,
    CD_STIPPLE = 2,
    CD_PATTERN = 3,
    CD_HOLLOW = 4

--****
-- === Text Alignment Constants
--

global constant
    CD_NORTH = 0,
    CD_SOUTH = 1,
    CD_EAST = 2,
    CD_WEST = 3,
    CD_NORTH_EAST = 4,
    CD_NORTH_WEST = 5,
    CD_SOUTH_EAST = 6,
    CD_SOUTH_WEST = 7,
    CD_CENTER = 8,
    CD_BASE_LEFT = 9,
    CD_BASE_CENTER = 10,
    CD_BASE_RIGHT = 11

--****
-- === Style Constants
--

global constant
    CD_PLAIN = 0,
    CD_BOLD = 1,
    CD_ITALIC = 2,
    CD_UNDERLINE = 4,
    CD_STRIKEOUT = 8,
    CD_BOLD_ITALIC = or_bits(CD_BOLD, CD_ITALIC)

--****
-- === Font Size Constants
--

global constant
    CD_SMALL = 8,
    CD_STANDARD = 12,
    CD_LARGE = 18

--****
-- === Canvas Capabilities Constants
--

global constant
    CD_CAP_NONE = #0,
    CD_CAP_FLUSH = #1,
    CD_CAP_CLEAR = #2,
    CD_CAP_PLAY = #4,
    CD_CAP_YAXIS = #8,
    CD_CAP_CLIPAREA = #10,
    CD_CAP_CLIPPOLY = #20,
    CD_CAP_REGION = #40,
    CD_CAP_RECT = #80,
    CD_CAP_CHORD = #100,
    CD_CAP_IMAGERGB = #200,
    CD_CAP_IMAGERGBA = #400,
    CD_CAP_IMAGEMAP = #800,
    CD_CAP_GETIMAGERGB = #1000,
    CD_CAP_IMAGESRV = #2000,
    CD_CAP_BACKGROUND = #4000,
    CD_CAP_BACKOPACITY = #8000,
    CD_CAP_WRITEMODE = #10000,
    CD_CAP_LINESTYLE = #20000,
    CD_CAP_LINEWITH = #40000,
    CD_CAP_FPRIMTIVES = #80000,
    CD_CAP_HATCH = #100000,
    CD_CAP_STIPPLE = #200000,
    CD_CAP_PATTERN = #400000,
    CD_CAP_FONT = #800000,
    CD_CAP_FONTDIM = #1000000,
    CD_CAP_TEXTSIZE = #2000000,
    CD_CAP_TEXTORIENTATION = #4000000,
    CD_CAP_PALETTE = #8000000,
    CD_CAP_LINECAP = #10000000,
    CD_CAP_LINEJOIN = #20000000,
    CD_CAP_ALL = #FFFFFFFF

--****
-- === Canvas Draw Play Constants
--

global constant
    CD_SIZECB = 0,
    CD_ABORT = 1,
    CD_CONTINUE = 0

--****
-- === Simulation Flag Constants
--

global constant
    CD_SIM_NONE = #0,
    CD_SIM_LINE = #1,
    CD_SIM_RECT = #2,
    CD_SIM_BOX = #4,
    CD_SIM_ARC = #8,
    CD_SIM_SECTOR = #10,
    CD_SIM_CHORD = #20,
    CD_SIM_POLYLINE = #40,
    CD_SIM_POLYGON = #80,
    CD_SIM_TEXT = #100,
    CD_SIM_ALL = #FFFF,
    CD_SIM_LINES = or_bits(CD_SIM_LINE, or_bits(CD_SIM_RECT, or_bits(CD_SIM_ARC, CD_SIM_POLYLINE))),
    CD_SIM_FILLS = or_bits(CD_SIM_BOX, or_bits(CD_SIM_SECTOR, or_bits(CD_SIM_CHORD, CD_SIM_POLYGON)))

--****
-- === Predefined Color Constants
--
-- These are simply for convenience
--

global constant
    CD_RED = #FF0000,
    CD_DARK_RED = #800000,
    CD_GREEN = #FF00,
    CD_DARK_GREEN = #8000,
    CD_BLUE = #FF,
    CD_DARK_BLUE = #80,
    CD_YELLOW = #FFFF00,
    CD_DARK_YELLOW = #808000,
    CD_MAGENTA = #FF00FF,
    CD_DARK_MAGENTA = #800080,
    CD_CYAN = #FFFF,
    CD_DARK_CYAN = #8080,
    CD_WHITE = #FFFFFF,
    CD_BLACK = #0,
    CD_DARK_GRAY = #808080,
    CD_GRAY = #C0C0C0

--****
-- === Conversion Factor Constants
--
-- These are simply for convenience
--

global constant
    --** 
-- Milimeters to Points (pt = CD_MM2PT * mm)
CD_MM2PT = 2.83465,

--**
-- Radians to Degrees (deg = CD_RAD2DEG * rad)
CD_RAD2DEG = 57.2958,

--**
-- Degrees to Radians (rad = CD_DEG2RAD * deg)
CD_DEG2RAD = 0.0174533

--****
-- === Version Information Routines

constant
    xcdVersion = define_c_func(hCd, "cdVersion", {},P),
    xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
    xcdVersionNumber = define_c_func(hCd, "cdVersionNumber", {},I)

global function cdVersion()
    return peek_string(c_func(xcdVersion, {}))
end function

global function cdVersionDate()
    return peek_string(c_func(xcdVersionDate, {}))
end function

global function cdVersionNumber()
    return c_func(xcdVersionNumber, {})
end function

--****
-- === Canvas Initialization Routines
--

constant
    xcdContextIup = define_c_func(hCdIup, "cdContextIup", {},P),
    xcdContextPrinter = define_c_func(hCd, "cdContextPrinter", {},P),
    xcdContextPS = define_c_func(hCd, "cdContextPS", {},P),
    xcdContextPicture = define_c_func(hCd, "cdContextPicture", {},P)

global constant
    CD_IUP = c_func(xcdContextIup, {}),
    CD_PRINTER = c_func(xcdContextPrinter, {}),
    CD_PS = c_func(xcdContextPS, {}),
    CD_PICTURE = c_func(xcdContextPicture, {})

ifdef WINDOWS then
constant
        xcdContextNativeWindow = define_c_func(hCd, "cdContextNativeWindow", {},P),
        xcdGetScreenSize = define_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
        xcdGetScreenColorPlanes = define_c_func(hCd, "cdGetScreenColorPlanes", {},I)

global constant
        CD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
end ifdef

global function cdGetScreenSize()
ifdef WINDOWS then
atom w = allocate(24)
atom h = w+4
atom w_mm = h+4
atom h_mm = w_mm+8

    c_proc(xcdGetScreenSize, {w, h, w_mm, h_mm})

    sequence data = peek4s({w, 2}) & peek_double({w_mm, 2})

    free(w)

    return data
elsedef
    return 0
end ifdef
end function

global function cdGetScreenColorPlanes()
ifdef WINDOWS then
    return c_func(xcdGetScreenColorPlanes, {})
elsedef
    return 0
end ifdef
end function

constant
    xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
    xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
    xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
    xcdCanvasActivate = define_c_func(hCd, "cdCanvasActivate", {P},I),
    xcdCanvasDeactivate = define_c_proc(hCd, "cdCanvasDeactivate", {P}),
    xcdUseContextPlus = define_c_func(hCd, "cdUseContextPlus", {I},I),
    xcdInitContextPlus = define_c_proc(hCd, "cdInitContextPlus", {})

global function cdCreateCanvas(atom hCdContext, object data)
atom fnVal, pData

    if sequence(data) then
        pData = allocate_string(data)
    else
        pData = data
    end if

    fnVal = c_func(xcdCreateCanvas, {hCdContext, pData})

    if sequence(data) then
        free(pData)
    end if

    return fnVal
end function

global procedure cdKillCanvas(atom hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

global function cdCanvasGetContext(atom hCdCanvas)
    return c_func(xcdCanvasGetContext, {hCdCanvas})
end function

global function canvas_activate(atom hCdCanvas)
    return c_func(xcdCanvasActivate, {hCdCanvas})
end function

global procedure cavas_deactivate(atom hCdCanvas)
    c_proc(xcdCanvasDeactivate, {hCdCanvas})
end procedure

global function use_context_plus(integer use)
    return c_func(xcdUseContextPlus, {use})
end function

global procedure init_context_plusun()
    c_proc(xcdInitContextPlus, {})
end procedure

--****
-- === Context Routines
--

constant
    xcdContextRegisterCallback = define_c_func(hCd, "cdContextRegisterCallback", {P,I,P},I),
    xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
    xcdCanvasPlay = define_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P},I)

global function context_register_callback(atom hCdContext, integer cb, integer cbFunc)
    return c_func(xcdContextRegisterCallback, {hCdContext, cb, cbFunc})
end function

global function cdContextCaps(atom hCdContext)
    return c_func(xcdContextCaps, {hCdContext})
end function

-- ??????
global function canvas_play(atom hCdCanvas, atom hCdContext, integer xmin, integer xmax, integer ymin, integer ymax, sequence data)
atom fnVal, pData

    pData = allocate_string(data)
    fnVal = c_func(xcdCanvasPlay, {hCdCanvas, hCdContext, xmin, xmax, ymin, ymax, pData})
    free(pData)
    return fnVal
end function

-----------------------------------------------------------------------------------------
--
-- control
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasSimulate = define_c_func(hCd, "cdCanvasSimulate", {P,I},I),
    xcdCanvasFlush = define_c_proc(hCd, "cdCanvasFlush", {P}),
    xcdCanvasClear = define_c_proc(hCd, "cdCanvasClear", {P}),
    xcdCanvasSaveState = define_c_func(hCd, "cdCanvasSaveState", {P},P),
    xcdCanvasRestoreState = define_c_proc(hCd, "cdCanvasRestoreState", {P,P}),
    xcdCanvasReleaseState = define_c_proc(hCd, "cdReleaseState", {P}),
    xcdCanvasSetAttribute = define_c_proc(hCd, "cdCanvasSetAttribute", {P,P,P}),
    --void     cdCanvasSetfAttribute(cdCanvas* canvas, const char* name, const char* format, ...);
    xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P)

global function canvas_simulate(atom hCdCanvas, integer mode)
    return c_func(xcdCanvasSimulate, {hCdCanvas, mode})
end function

global procedure canvas_flush(atom hCdCanvas)
    c_proc(xcdCanvasFlush, {hCdCanvas})
end procedure

global procedure canvas_clear(atom hCdCanvas)
    c_proc(xcdCanvasClear, {hCdCanvas})
end procedure

global function canvas_save_state(atom hCdCanvas)
    return c_func(xcdCanvasSaveState, {hCdCanvas})
end function

global procedure canvas_restore_state(atom hCdCanvas, atom hCdState)
    c_proc(xcdCanvasRestoreState, {hCdCanvas, hCdState})
end procedure

global procedure canvas_release_state(atom hCdState)
    c_proc(xcdCanvasReleaseState, {hCdState})
end procedure

global procedure canvas_set_attribute(atom hCdCanvas, sequence name, sequence data)
atom pName, pData

    pName = allocate_string(name)
    pData = allocate_string(data)
    c_proc(xcdCanvasSetAttribute, {hCdCanvas, pName, pData})
    free(pName)
    free(pData)
end procedure

global function cdCanvasGetAttribute(atom hCdCanvas, sequence name)
atom fnVal, pName
    pName = allocate_string(name)
    fnVal = c_func(xcdCanvasGetAttribute, {hCdCanvas, pName})
    free(pName)
    if not fnVal then return NULL end if
    return peek_string(fnVal)
end function

------------------------------------------------------------------------------------------
--
--  coordinate transformation 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
    xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
    xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
    xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
    xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
    xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
    xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
    xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
    xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
    xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
    xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
    xcdCanvasGetOrigin = define_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
    xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
    xcdCanvasTransform = define_c_proc(hCd, "cdCanvasTransform", {P,P}),
    xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
    xcdCanvasTransformMultiply = define_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
    xcdCanvasTransformRotate = define_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
    xcdCanvasTransformScale = define_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
    xcdCanvasTransformTranslate = define_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
    xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
    xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P})

global function cdCanvasGetSize(atom hCdCanvas)
atom pWidth, pHeight, pWidth_mm, pHeight_mm
sequence size
    pWidth = allocate(2*4+2*8)
    pHeight = pWidth+4
    pWidth_mm = pHeight+4
    pHeight_mm = pWidth_mm+8
    c_proc(xcdCanvasGetSize, {hCdCanvas, pWidth, pHeight, pWidth_mm, pHeight_mm})
    size = peek4s({pWidth, 2}) & peek_double({pWidth_mm, 2})
    free(pWidth)
    return size
end function

global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom fnVal, pY
    pY = allocate(4)
    poke4(pY, y)
    fnVal = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
    free(pY) --peek4s(pY)==fnVal
    return fnVal
end function

global function cdfCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom fnVal, pY
    pY = allocate(8)
    poke_double(pY, y)
    fnVal = c_func(xcdfCanvasUpdateYAxis, {hCdCanvas, pY})
    free(pY) --peek_double(pY)=fnVal
    return fnVal
end function

global function cdCanvasInvertYAxis(atom hCdCanvas, atom y)
    return c_func(xcdCanvasInvertYAxis, {hCdCanvas, y})
end function

global function cdfCanvasInvertYAxis(atom hCdCanvas, atom y)
    return c_func(xcdfCanvasInvertYAxis, {hCdCanvas, y})
end function

global function cdCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(8)
    pDy = pDx+4
    c_proc(xcdCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peek4s({pDx, 2})
    free(pDx)
    return dx_dy
end function

global function cdCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

global function cdfCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(16)
    pDy = pDx+8
    c_proc(xcdfCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peek_double({pDx, 2})
    free(pDx)
    return dx_dy
end function

global function cdfCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdfCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasOrigin, {hCdCanvas, x, y})
end procedure

global procedure f_canvas_origin(atom hCdCanvas, atom x, atom y)
    c_proc(xcdfCanvasOrigin, {hCdCanvas, x, y})
end procedure

global function cdCanvasGetOrigin(atom hCdCanvas)
atom pX, pY
sequence origin

    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasGetOrigin, {hCdCanvas, pX, pY})
    origin = peek4s({pX, 2})
    free(pX)
    return origin
end function

global function f_canvas_get_origin(atom hCdCanvas)
atom pX, pY
sequence origin

    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasGetOrigin, {hCdCanvas, pX, pY})
    origin = peek_double({pX, 2})
    free(pX)
    return origin
end function

global procedure canvas_transform(atom hCdCanvas, sequence matrix)
atom pMatrix
    pMatrix = allocate(6*8)
    poke_double(pMatrix, matrix)
    c_proc(xcdCanvasTransform, {hCdCanvas, pMatrix})
    free(pMatrix)
end procedure

global function cdCanvasGetTransform(atom hCdCanvas)
atom fnVal
    fnVal = c_func(xcdCanvasGetTransform, {hCdCanvas})
    return peek_double({fnVal, 6})
end function

global procedure canvas_transform_multiply(atom hCdCanvas, sequence matrix)
atom pMatrix
    pMatrix = allocate(6*8)
    poke_double(pMatrix, matrix)
    c_proc(xcdCanvasTransformMultiply, {hCdCanvas, pMatrix})
    free(pMatrix)
end procedure

global procedure canvas_transform_rotate(atom hCdCanvas, atom angle)
    c_proc(xcdCanvasTransformRotate, {hCdCanvas, angle})
end procedure

global procedure canvas_transform_scale(atom hCdCanvas, atom sx, atom sy)
    c_proc(xcdCanvasTransformScale, {hCdCanvas, sx, sy})
end procedure

global procedure canvas_transform_translate(atom hCdCanvas, atom dx, atom dy)
    c_proc(xcdCanvasTransformTranslate, {hCdCanvas, dx, dy})
end procedure

global function cdCanvasTransformPoint(atom hCdCanvas, atom x, atom y)
atom pX, pY
sequence tx_ty
    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
    tx_ty = peek4s({pX, 2})
    free(pX)
    return tx_ty
end function

global function f_canvas_transform_point(atom hCdCanvas, atom x, atom y)
atom pX, pY
sequence tx_ty
    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
    tx_ty = peek_double({pX, 2})
    free(pX)
    return tx_ty
end function

------------------------------------------------------------------------------------------
--
--   clipping 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasClip = define_c_func(hCd, "cdCanvasClip", {P,I},I),
    xcdCanvasClipArea = define_c_proc(hCd, "cdCanvasClipArea", {P,I,I,I,I}),
    xcdfCanvasClipArea = define_c_proc(hCd, "cdfCanvasClipArea", {P,D,D,D,D}),
    xcdCanvasGetClipArea = define_c_func(hCd, "cdCanvasGetClipArea", {P,P,P,P,P},I),
    xcdfCanvasGetClipArea = define_c_func(hCd, "cdfCanvasGetClipArea", {P,P,P,P,P},I)

global function canvas_clip(atom hCdCanvas, integer mode)
    return c_func(xcdCanvasClip, {hCdCanvas, mode})
end function --cdCanvasClip

global procedure canvas_clip_area(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure f_canvas_clip_area(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdfCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function canvas_get_clip_area(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence area
    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = peek4s({pXmin, 4})
    free(pXmin)
    return area
end function

global function f_canvas_get_clip_area(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence area
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xcdfCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = peek_double({pXmin, 4})
    free(pXmin)
    return area
end function

------------------------------------------------------------------------------------------
--
--  clipping region 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasIsPointInRegion = define_c_func(hCd, "cdCanvasIsPointInRegion", {P,I,I},I),
    xcdCanvasOffsetRegion = define_c_proc(hCd, "cdCanvasOffsetRegion", {P,I,I}),
    xcdCanvasGetRegionBox = define_c_proc(hCd, "cdCanvasGetRegionBox", {P,P,P,P,P,P}),
    xcdCanvasRegionCombineMode = define_c_func(hCd, "cdCanvasRegionCombineMode", {P,I},I)

global function canvas_is_point_in_region(atom hCdCanvas, atom x, atom y)
    return c_func(xcdCanvasIsPointInRegion, {hCdCanvas, x, y})
end function

global procedure canvas_offset_region(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasOffsetRegion, {hCdCanvas, x, y})
end procedure

global function canvas_get_region_box(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence box

    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetRegionBox, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

global procedure canvas_region_combine_mode(atom hCdCanvas, integer mode)
    c_proc(xcdCanvasRegionCombineMode, {hCdCanvas, mode})
end procedure

------------------------------------------------------------------------------------------
--
-- drawing primitives
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasPixel = define_c_proc(hCd, "cdCanvasPixel", {P,I,I}),
    xcdCanvasMark = define_c_proc(hCd, "cdCanvasMark", {P,I,I}),
    xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
    xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
    xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
    xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
    xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
    xcdCanvasBox = define_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I}),
    xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
    xcdCanvasSector = define_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D}),
    xcdCanvasChord = define_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D}),
    xcdCanvasText = define_c_proc(hCd, "cdCanvasText", {P,I,I,P})

global procedure canvas_pixel(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasPixel, {hCdCanvas, x, y})
end procedure

global procedure canvas_mark(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasMark, {hCdCanvas, x, y})
end procedure

global procedure cdCanvasLine(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasLine, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasBegin(atom hCdCanvas, integer mode)
    c_proc(xcdCanvasBegin, {hCdCanvas, mode})
end procedure

global procedure cdCanvasVertex(atom hCdCanvas, atom x, atom y)
--PL hCd is the open_dll...
--  c_proc(xcdCanvasVertex, { hCd, x, y })
    c_proc(xcdCanvasVertex, {hCdCanvas, x, y})
end procedure

global procedure cdCanvasEnd(atom hCdCanvas)
    c_proc(xcdCanvasEnd, {hCdCanvas})
end procedure

global procedure cdCanvasRect(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure canvas_box(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasBox, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasArc(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure canvas_sector(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasSector, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure canvas_chord(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasChord, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure canvas_text(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdCanvasText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-----------------------------------------------------------------------------------------
--
-- primitives with double as arguments instead of integer
--
-----------------------------------------------------------------------------------------
constant
    xcdfCanvasLine = define_c_proc(hCd, "cdfCanvasLine", {P,D,D,D,D}),
    xcdfCanvasVertex = define_c_proc(hCd, "cdfCanvasVertex", {P,D,D}),
    xcdfCanvasRect = define_c_proc(hCd, "cdfCanvasRect", {P,D,D,D,D}),
    xcdfCanvasBox = define_c_proc(hCd, "cdfCanvasBox", {P,D,D,D,D}),
    xcdfCanvasArc = define_c_proc(hCd, "cdfCanvasArc", {P,D,D,D,D,D,D}),
    xcdfCanvasSector = define_c_proc(hCd, "cdfCanvasSector", {P,D,D,D,D,D,D}),
    xcdfCanvasChord = define_c_proc(hCd, "cdfCanvasChord", {P,D,D,D,D,D,D}),
    xcdfCanvasText = define_c_proc(hCd, "cdfCanvasText", {P,D,D,P})

global procedure f_canvas_line(atom hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdfCanvasLine, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure f_canvas_vertex(atom hCdCanvas, atom x1, atom y1)
    c_proc(xcdfCanvasVertex, {hCdCanvas, x1, y1})
end procedure

global procedure f_canvas_rect(atom hCdCanvas, atom xmin, atom ymin, atom xmax, atom ymax)
    c_proc(xcdfCanvasRect, {hCdCanvas, xmin, ymin, xmax, ymax})
end procedure

global procedure f_canvas_box(atom hCdCanvas, atom xmin, atom ymin, atom xmax, atom ymax)
    c_proc(xcdfCanvasBox, {hCdCanvas, xmin, ymin, xmax, ymax})
end procedure

global procedure f_canvas_arc(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasArc, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_sector(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasSector, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_chord(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasChord, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_text(atom hCdCanvas, atom x1, atom y1, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdfCanvasText, {hCdCanvas, x1, y1, pText})
    free(pText)
end procedure

------------------------------------------------------------------------------------------
--
-- attributes
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasSetBackground = define_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
    xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
    xcdCanvasBackground = define_c_func(hCd, "cdCanvasBackground", {P,L},L),
    xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
    xcdCanvasBackOpacity = define_c_func(hCd, "cdCanvasBackOpacity", {P,I},I),
    xcdCanvasWriteMode = define_c_func(hCd, "cdCanvasWriteMode", {P,I},I),
    xcdCanvasLineStyle = define_c_func(hCd, "cdCanvasLineStyle", {P,I},I),
    xcdCanvasLineStyleDashes = define_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P,I}),
    xcdCanvasLineWidth = define_c_func(hCd, "cdCanvasLineWidth", {P,I},I),
    xcdCanvasLineJoin = define_c_func(hCd, "cdCanvasLineJoin", {P,I},I),
    xcdCanvasLineCap = define_c_func(hCd, "cdCanvasLineCap", {P,I},I),
    xcdCanvasInteriorStyle = define_c_func(hCd, "cdCanvasInteriorStyle", {P,I},I),
    xcdCanvasHatch = define_c_func(hCd, "cdCanvasHatch", {P,I},I),
    xcdCanvasStipple = define_c_proc(hCd, "cdCanvasStipple", {P,I,I,P}),
    xcdCanvasGetStipple = define_c_func(hCd, "cdCanvasGetStipple", {P,P,P},P),
    xcdCanvasPattern = define_c_proc(hCd, "cdCanvasPattern", {P,I,I,P}),
    xcdCanvasGetPattern = define_c_func(hCd, "cdCanvasGetPattern", {P,P,P},P),
    xcdCanvasFillMode = define_c_func(hCd, "cdCanvasFillMode", {P,I},I),
    xcdCanvasFont = define_c_func(hCd, "cdCanvasFont", {P,P,I,I},I),
    xcdCanvasGetFont = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
    xcdCanvasNativeFont = define_c_func(hCd, "cdCanvasNativeFont", {P,P},P),
    xcdCanvasTextAlignment = define_c_func(hCd, "cdCanvasTextAlignment", {P,I},I),
    xcdCanvasTextOrientation = define_c_func(hCd, "cdCanvasTextOrientation", {P,D},D),
    xcdCanvasMarkType = define_c_func(hCd, "cdCanvasMarkType", {P,I},I),
    xcdCanvasMarkSize = define_c_func(hCd, "cdCanvasMarkSize", {P,I},I)

global procedure canvas_set_background(atom hCdCanvas, atom color)
    c_proc(xcdCanvasSetBackground, {hCdCanvas, color})
end procedure

global procedure cdCanvasSetForeground(atom hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function canvas_background(atom hCdCanvas, atom color)
    return c_func(xcdCanvasBackground, {hCdCanvas, color})
end function

global function canvas_foreground(atom hCdCanvas, atom color)
    return c_func(xcdCanvasForeground, {hCdCanvas, color})
end function

global function canvas_back_opacity(atom hCdCanvas, atom opacity)
    return c_func(xcdCanvasBackOpacity, {hCdCanvas, opacity})
end function

global function canvas_write_mode(atom hCdCanvas, atom mode)
    return c_func(xcdCanvasWriteMode, {hCdCanvas, mode})
end function

global function cdCanvasLineStyle(atom hCdCanvas, atom style)
    return c_func(xcdCanvasLineStyle, {hCdCanvas, style})
end function

global procedure cd_canvas_line_style_dashes(atom hCdCanvas, sequence dashes, integer count)
atom pDashes
    pDashes = allocate(4*length(dashes))
    poke4(pDashes, dashes)
    c_proc(xcdCanvasLineStyleDashes, {hCdCanvas, pDashes, count})
    free(pDashes)
end procedure

global function cdCanvasLineWidth(atom hCdCanvas, atom width)
    return c_func(xcdCanvasLineWidth, {hCdCanvas, width})
end function

global function canvas_line_join(atom hCdCanvas, atom join)
    return c_func(xcdCanvasLineJoin, {hCdCanvas, join})
end function

global function canvas_line_cap(atom hCdCanvas, atom cap)
    return c_func(xcdCanvasLineCap, {hCdCanvas, cap})
end function

global function canvas_interior_style(atom hCdCanvas, atom style)
    return c_func(xcdCanvasInteriorStyle, {hCdCanvas, style})
end function

global function canvas_hatch(atom hCdCanvas, atom style)
    return c_func(xcdCanvasHatch, {hCdCanvas, style})
end function

global procedure canvas_stipple(atom hCdCanvas, atom width, atom height, sequence stipple)
atom pStipple

    pStipple = allocate(length(stipple))
    poke(pStipple, stipple)
    c_proc(xcdCanvasStipple, {hCdCanvas, width, height, pStipple})
    free(pStipple)
end procedure

global function canvas_get_stipple(atom hCdCanvas)
atom fnVal, pW, pH
sequence w_h, stipple
    pW = allocate(8)
    pH = pW+4
    fnVal = c_func(xcdCanvasGetStipple, {hCdCanvas, pW, pH})
    w_h = peek4s({pW, 2})
    free(pW)
    stipple = peek({fnVal, w_h[1]*w_h[2]})
    return {{w_h}, stipple}
end function

global procedure canvas_pattern(atom hCdCanvas, atom width, atom height, sequence pattern)
atom pPattern

    pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xcdCanvasPattern, {hCdCanvas, width, height, pPattern})
    free(pPattern)
end procedure

global function canvas_get_pattern(atom hCdCanvas)
atom fnVal, pW, pH
sequence w_h, pattern
    pW = allocate(8)
    pH = pW+4
    fnVal = c_func(xcdCanvasGetPattern, {hCdCanvas, pW, pH})
    w_h = peek4s({pW, 2})
    free(pW)
    pattern = peek4s({fnVal, w_h[1]*w_h[2]})
    return {{w_h}, pattern}
end function

global function canvas_fill_mode(atom hCdCanvas, atom mode)
    return c_func(xcdCanvasFillMode, {hCdCanvas, mode})
end function

global function cdCanvasFont(atom hCdCanvas, sequence font, atom style, atom size)
atom fnVal, pFont

    pFont = allocate_string(font)
    fnVal = c_func(xcdCanvasFont, {hCdCanvas, pFont, style, size})
    free(pFont)
    return fnVal
end function

global function cdCanvasGetFont(atom hCdCanvas)
atom pFont, pSize, pStyle
sequence font
    pStyle = allocate(1024)
    pSize = pStyle+4
    pFont = pSize+4
    c_proc(xcdCanvasGetFont, {hCdCanvas, pFont, pStyle, pSize})
    font = {peek_string(pFont)} & peek4s({pStyle, 2})
    free(pStyle)
    return font
end function

global function canvas_native_font(atom hCdCanvas, sequence font)
atom pFont, fnVal
    if length(font) then
        pFont = allocate_string(font)
    else
        pFont = 0
    end if
    fnVal = c_func(xcdCanvasNativeFont, {hCdCanvas, pFont})
    font = peek_string(fnVal)
    if pFont then free(pFont) end if
    return font
end function

global function canvas_text_alignment(atom hCdCanvas, atom alignment)
    return c_func(xcdCanvasTextAlignment, {hCdCanvas, alignment})
end function

global function canvas_text_orientation(atom hCdCanvas, atom angle)
    return c_func(xcdCanvasTextOrientation, {hCdCanvas, angle})
end function

global function canvas_mark_type(atom hCdCanvas, atom mtype)
    return c_func(xcdCanvasMarkType, {hCdCanvas, mtype})
end function

global function canvas_mark_size(atom hCdCanvas, atom msize)
    return c_func(xcdCanvasMarkSize, {hCdCanvas, msize})
end function

-----------------------------------------------------------------------------------------
--
--  vector text
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasVectorText = define_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
    xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P})

global procedure canvas_vector_text(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

global procedure cdCanvasMultiLineVectorText(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-----------------------------------------------------------------------------------------
--
-- vector text attributes
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
    xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
    xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
    xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I)

global function cdCanvasVectorFont(atom hCdCanvas, sequence font)
atom pFont = 0

    if length(font) then
        pFont = allocate_string(font)
    end if

    atom fnVal = c_func(xcdCanvasVectorFont, {hCdCanvas, pFont})

    font = peek_string(fnVal)
    if pFont then
        free(pFont)
    end if

    return font
end function

global procedure cdCanvasVectorTextDirection(atom hCdCanvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global function canvas_vector_text_transform(atom hCdCanvas, sequence matrix)
atom fnVal, pMatrix

    pMatrix = allocate(8*6)
    poke_double(pMatrix, matrix)
    fnVal = c_func(xcdCanvasVectorTextTransform, {hCdCanvas, pMatrix})
    matrix = peek_double({fnVal, 6})
    free(pMatrix)
    return matrix
end function

global procedure canvas_vector_text_size(atom hCdCanvas, atom w, atom h, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdCanvasVectorTextSize, {hCdCanvas, w, h, pText})
    free(pText)
end procedure

global function canvas_vector_char_size(atom hCdCanvas, atom size)
    return c_func(xcdCanvasVectorCharSize, {hCdCanvas, size})
end function

-----------------------------------------------------------------------------------------
--
--  vector text properties 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
    xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P})

global function canvas_get_vector_text_size(atom hCdCanvas, sequence text)
atom pText, pX, pY
sequence x_y
    pText = allocate_string(text)
    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasGetVectorTextSize, {hCdCanvas, pText, pX, pY})
    x_y = peek4s({pX, 2})
    free(pText)
    free(pX)
    return x_y
end function

global function cdCanvasGetVectorTextBounds(atom hCdCanvas, sequence text, atom px, atom py)
atom pRect, pText
sequence rect
    pRect = allocate(8*4)
    pText = allocate_string(text)
    c_proc(xcdCanvasGetVectorTextBounds, {hCdCanvas, pText, px, py, pRect})
    rect = peek4s({pRect, 8})
    free(pRect)
    free(pText)
    return rect
end function

-----------------------------------------------------------------------------------------
--
-- properties --
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
    xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
    xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
    xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
    xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I)

global function cdCanvasGetFontDim(atom hCdCanvas)
atom pW, pH, pA, pD
sequence font_metrics
    pW = allocate(16)
    pH = pW+4
    pA = pH+4
    pD = pA+4
    c_proc(xcdCanvasGetFontDim, {hCdCanvas, pW, pH, pA, pD})
    font_metrics = peek4s({pW, 4})
    free(pW)
    return font_metrics
end function

global function cdCanvasGetTextSize(atom hCdCanvas, sequence text)
atom pW, pH, pText
sequence text_size

    pW = allocate(8+length(text)+1)
    pH = pW+4
    pText = pH+4
    poke(pText, text & 0)
    c_proc(xcdCanvasGetTextSize, {hCdCanvas, pText, pW, pH})
    text_size = peek4s({pW, 2})
    free(pW)
    return text_size
end function

global function cdCanvasGetTextBox(atom hCdCanvas, atom x, atom y, sequence text)
atom pText, pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(16+length(text)+1)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    pText = pYmax+4
    poke(pText, text & 0)
    c_proc(xcdCanvasGetTextBox, {hCdCanvas, x, y, pText, pXmin, pXmax, pYmin, pYmax})
    box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

global function cdCanvasGetTextBounds(atom hCdCanvas, atom x, atom y, sequence text)
atom pText, pRect
sequence bounds

    pRect = allocate(32+length(text)+1)
    pText = pRect+32
    poke(pText, text & 0)
    c_proc(xcdCanvasGetTextBounds, {hCdCanvas, x, y, pText, pRect})
    bounds = peek4s({pRect, 8})
    free(pRect)
    return bounds
end function

global function cdCanvasGetColorPlanes(atom hCdCanvas)
    return c_func(xcdCanvasGetColorPlanes, {hCdCanvas})
end function

-----------------------------------------------------------------------------------------
--
-- color 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasPalette = define_c_proc(hCd, "cdCanvasPalette", {P,I,P,I})

global procedure canvas_palette(atom hCdCanvas, sequence palette, integer mode)
atom pPalette

    pPalette = allocate(4*length(palette))
    poke4(pPalette, palette)
    c_proc(xcdCanvasPalette, {hCdCanvas, length(palette), pPalette, mode})
end procedure

-----------------------------------------------------------------------------------------
--
-- client images 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
    xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
    xcdCanvasPutImageRectRGBA = define_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I}),
    xcdCanvasPutImageRectMap = define_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I})

global function cdCanvasGetImageRGB(atom hCdCanvas, atom x, atom y, atom w, atom h)
atom pR, pG, pB
sequence rgb

    pR = allocate(3*w*h)
    pG = pR+w*h
    pB = pG+w*h
    c_proc(xcdCanvasGetImageRGB, {hCdCanvas, pR, pG, pB, x, y, w, h})
    rgb = repeat({}, 3)
    rgb[1] = peek({pR, w*h})
    rgb[2] = peek({pG, w*h})
    rgb[3] = peek({pB, w*h})
    free(pR)
    return rgb
end function

global procedure cdCanvasPutImageRectRGB(atom hCdCanvas, atom iw, atom ih,
        sequence rgb, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)
atom pR, pG, pB

    pR = allocate(3*length(rgb[1]))
    pG = pR+length(rgb[1])
    pB = pG+length(rgb[1])
    poke(pR, rgb[1])
    poke(pG, rgb[2])
    poke(pB, rgb[3])
    c_proc(xcdCanvasPutImageRectRGB, {hCdCanvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure canvas_put_image_rect_rgba(atom hCdCanvas, atom iw, atom ih,
        sequence rgba, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pR, pG, pB, pA

    pR = allocate(4*length(rgba[1]))
    pG = pR+length(rgba[1])
    pB = pG+length(rgba[1])
    pA = pB+length(rgba[1])
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xcdCanvasPutImageRectRGBA, {hCdCanvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure canvas_put_image_rect_map(atom hCdCanvas, atom iw, atom ih,
        sequence map, sequence colors,
        atom x, atom y, atom w, atom h,
        atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pIndex, pColors
    pColors = allocate(4*256+length(map))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, map)
    c_proc(xcdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

-----------------------------------------------------------------------------------------
--
-- server images 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasCreateImage = define_c_func(hCd, "cdCanvasCreateImage", {P,I,I},P),
    xcdKillImage = define_c_proc(hCd, "cdKillImage", {P}),
    xcdCanvasGetImage = define_c_proc(hCd, "cdCanvasGetImage", {P,P,I,I}),
    xcdCanvasPutImageRect = define_c_proc(hCd, "CdCanvasPutImageRect", {P,P,I,I,I,I,I,I}),
    xcdCanvasScrollArea = define_c_proc(hCd, "CdCanvasScrollArea", {P,I,I,I,I,I,I})

global function canvas_create_image(atom hCdCanvas, atom w, atom h)
    return c_func(xcdCanvasCreateImage, {hCdCanvas, w, h})
end function

global procedure kill_image(atom hCdImage)
    c_proc(xcdKillImage, {hCdImage})
end procedure

global procedure canvas_get_image(atom hCdCanvas, atom hCdImage, atom x, atom y)
    c_proc(xcdCanvasGetImage, {hCdCanvas, hCdImage, x, y})
end procedure

global procedure canvas_put_image_rect(atom hCdCanvas, atom hCdImage, atom x, atom y,
        atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasPutImageRect, {hCdCanvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure

global procedure canvas_scroll_area(atom hCdCanvas, atom xmin, atom xmax,
        atom ymin, atom ymax, atom dx, atom dy)
    c_proc(xcdCanvasScrollArea, {hCdCanvas, xmin, xmax, ymin, ymax, dx, dy})
end procedure

-----------------------------------------------------------------------------------------
--
-- bitmap 
--
-----------------------------------------------------------------------------------------
constant
    xcdCreateBitmap = define_c_func(hCd, "cdCreateBitmap", {I,I,I},P),
    xcdInitBitmapRGB = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P},P), -- type CD_RGB
    xcdInitBitmapRGBA = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P,P},P), -- type CD_RGBA
--PL unused
--  xcdInitBitmapMAP = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P},P), -- type CD_MAP
    xcdKillBitmap = define_c_proc(hCd, "cdKillBitmap", {P}),
    xcdBitmapGetData = define_c_func(hCd, "cdBitmapGetData", {P,I},P),
    xcdBitmapSetRect = define_c_proc(hCd, "CdBitmapSetRect", {P,I,I,I,I}),
    xcdCanvasPutBitmap = define_c_proc(hCd, "cdCanvasPutBitmap", {P,P,I,I,I,I}),
    xcdCanvasGetBitmap = define_c_proc(hCd, "cdCanvasGetBitmap", {P,P,I,I}),
    xcdBitmapRGB2Map = define_c_proc(hCd, "cdBitmapRGB2Map", {P,P})

global function canvas_create_bitmap(atom w, atom h, integer btype)
    return c_func(xcdCreateBitmap, {w, h, btype})
end function

global function init_bitmap(atom w, atom h, integer btype, sequence data)
atom hCdBitmap, pR, pG, pB, pA, pIndex, pColors

    hCdBitmap = 0
    if btype=CD_RGB then
        pR = allocate(3*length(data[1]))
        pG = pR+length(data[1])
        pB = pG+length(data[1])
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        hCdBitmap = c_func(xcdInitBitmapRGB, {w, h, btype, pR, pG, pB})
        free(pR)
    elsif btype=CD_RGBA then
        pR = allocate(4*length(data[1]))
        pG = pR+length(data[1])
        pB = pG+length(data[1])
        pA = pB+length(data[1])
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        poke(pA, data[4])
        hCdBitmap = c_func(xcdInitBitmapRGBA, {w, h, btype, pR, pG, pB, pA})
        free(pR)
    elsif btype=CD_MAP then
        pColors = allocate(4*256+length(data[1]))
        pIndex = pColors+4*256
        poke4(pColors, data[2])
        poke(pIndex, data[1])
        ?9/0    -- DEV: clearly there is a missing c_func(xcdInitBitmapMAP... here
        free(pColors)
    end if
    return hCdBitmap
end function

global procedure kill_bitmap(atom hCdBitmap)
    c_proc(xcdKillBitmap, {hCdBitmap})
end procedure

global function bitmap_get_data(atom hCdBitmap, integer dataptr)
    return c_func(xcdBitmapGetData, {hCdBitmap, dataptr})
end function

global procedure bitmap_set_rect(atom hCdBitmap, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdBitmapSetRect, {hCdBitmap, xmin, xmax, ymin, ymax})
end procedure

global procedure canvas_put_bitmap(atom hCdCanvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xcdCanvasPutBitmap, {hCdCanvas, hCdBitmap, x, y, w, h})
end procedure

global procedure canvas_get_bitmap(atom hCdCanvas, atom hCdBitmap, atom x, atom y)
    c_proc(xcdCanvasGetBitmap, {hCdCanvas, hCdBitmap, x, y})
end procedure

global procedure bitmap_rgb_2_map(atom hCdBitmapRGB, atom hCdBitmapMAP)
    c_proc(xcdBitmapRGB2Map, {hCdBitmapRGB, hCdBitmapMAP})
end procedure

-----------------------------------------------------------------------------------------
--
-- color 
--
-----------------------------------------------------------------------------------------
constant
    xcdEncodeColor = define_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L),
    xcdDecodeColor = define_c_proc(hCd, "cdDecodeColor", {L,P,P,P}),
    xcdDecodeAlpha = define_c_func(hCd, "cdDecodeAlpha", {L},UC),
    xcdEncodeAlpha = define_c_func(hCd, "CdEncodeAlpha", {L,UC},L),
    xcdRGB2Map = define_c_proc(hCd, "cdRGB2Map", {I,I,P,P,P,P,I,P})

global function encode_color(atom red, atom green, atom blue)
    return c_func(xcdEncodeColor, {red, green, blue})
end function

global function decode_color(atom color)
atom pR, pG, pB
sequence rgb_tuple
    pR = allocate(12)
    pG = pR+4
    pB = pG+4
    c_proc(xcdDecodeColor, {color, pR, pG, pB})
    rgb_tuple = peek({pR, 3})
    free(pR)
    return rgb_tuple
end function

global function decode_alpha(atom color)
    return c_func(xcdDecodeAlpha, {color})
end function

global function encode_alpha(atom color, integer alpha)
    return c_func(xcdEncodeAlpha, {color, alpha})
end function

global function alpha(atom color)
    color = and_bits(color, #FF000000)
    color = floor(color/#1000000)
    return 255-color
end function

global function red(atom color)
    color = and_bits(color, #FF0000)
    return color/power(2, 16)
end function

global function green(atom color)
    color = and_bits(color, #FF00)
    return color/256
end function

global function blue(atom color)
    return remainder(color, 256)
end function

global function rgb_2_map(atom w, atom h, sequence rgb, integer pal_size)
atom pR, pG, pB, pI, pC
sequence smap

    if sequence(rgb) then end if    --DEV PL unused
    pR = allocate(4*w*h+1024)
    pG = pR+w*h
    pB = pG+w*h
    pI = pB+w*h
    pC = pI+w*h
    c_proc(xcdRGB2Map, {w, h, pR, pG, pB, pI, pal_size, pC})
    smap = {peek({pI, w*h}), peek4s({pC, pal_size})}
    free(pR)
    return smap
end function

-----------------------------------------------------------------------------------------
--
--  world coordinates
--
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
--
--  coordinate transformation
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
    xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
    xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
    xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
    xwdCanvasWorld2Canvas = define_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P}),
    xwdCanvasWorld2CanvasSize = define_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P}),
    xwdCanvasCanvas2World = define_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D})

global procedure wdCanvasWindow(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetWindow(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdWindow

    pXmin = allocate(4*8)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetWindow, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    wdWindow = peek_double({pXmin, 4})
    free(pXmin)
    return wdWindow
end function

global procedure wdCanvasViewport(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasViewport, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetViewport(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdViewport

    pXmin = allocate(4*4)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xwdCanvasGetViewport, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    wdViewport = peek4s({pXmin, 4})
    free(pXmin)
    return wdViewport
end function

global function wd_canvas_world2_canvas(atom hCdCanvas, atom xw, atom yw)
atom pX, pY
sequence xy

    pX = allocate(2*4)
    pY = pX+4
    c_proc(xwdCanvasWorld2Canvas, {hCdCanvas, xw, yw, pX, pY})
    xy = peek4s({pX, 2})
    free(pX)
    return xy
end function

global function wd_canvas_world2_canvas_size(atom hCdCanvas, atom ww, atom hw)
atom pW, pH
sequence wh

    pW = allocate(8)
    pH = pW+4
    c_proc(xwdCanvasWorld2CanvasSize, {hCdCanvas, ww, hw, pW, pH})
    wh = peek4s({pW, 2})
    free(pW)
    return wh
end function

global function wd_canvas_canvas2_world(atom hCdCanvas, atom xv, atom yv)
atom pWx, pWy
sequence xy

    pWx = allocate(2*8)
    pWy = pWx+8
    c_proc(xwdCanvasCanvas2World, {hCdCanvas, xv, yv, pWx, pWy})
    xy = peek_double({pWx, 2})
    free(pWx)
    return xy
end function

-----------------------------------------------------------------------------------------
--
-- clipping region
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasClipArea = define_c_proc(hCd, "wdCanvasClipArea", {P,D,D,D,D}),
    xwdCanvasGetClipArea = define_c_func(hCd, "wdCanvasGetclipArea", {P,P,P,P,P},I),
    xwdCanvasIsPointInRegion = define_c_func(hCd, "wdCanvasIsPointInRegion", {P,D,D},I),
    xwdCanvasOffsetRegion = define_c_proc(hCd, "wdCanvasOffsetRegion", {P,D,D}),
    xwdCanvasGetRegionBox = define_c_proc(hCd, "wdCanvasGetRegionBox", {P,P,P,P,P}),
    xwdCanvasHardcopy = define_c_proc(hCd, "wdCanvasHardCopy", {P,P,P,P})

global procedure wd_canvas_clip_area(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wd_canvas_get_clip_area(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
integer clipping_status
sequence area

    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    clipping_status = c_func(xwdCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = peek_double({pXmin, 4})
    free(pXmin)
    return clipping_status & area
end function

global function wd_canvas_is_point_in_region(atom hCdCanvas, atom x, atom y)
    return c_func(xwdCanvasIsPointInRegion, {hCdCanvas, x, y})
end function

global procedure wd_canvas_offset_region(atom hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasOffsetRegion, {hCdCanvas, x, y})
end procedure

global function wd_canvas_get_region_box(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence box

    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetRegionBox, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    box = peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global procedure wd_canvas_hardcopy(atom hCdCanvas, atom hCdContext, atom pData, atom cbFct)
    c_proc(xwdCanvasHardcopy, {hCdCanvas, hCdContext, pData, cbFct})
end procedure

-----------------------------------------------------------------------------------------
--
--  world draw primitives
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasPixel = define_c_proc(hCd, "wdCanvasPixel", {P,D,D,L}),
    xwdCanvasMark = define_c_proc(hCd, "wdCanvasMark", {P,D,D}),
    xwdCanvasLine = define_c_proc(hCd, "wdCanvasLine", {P,D,D,D,D}),
    xwdCanvasVertex = define_c_proc(hCd, "wdCanvasVertex", {P,D,D}),
    xwdCanvasRect = define_c_proc(hCd, "wdCanvasRect", {P,D,D,D,D}),
    xwdCanvasBox = define_c_proc(hCd, "wdCanvasBox", {P,D,D,D,D}),
    xwdCanvasArc = define_c_proc(hCd, "wdCanvasArc", {P,D,D,D,D,D,D}),
    xwdCanvasSector = define_c_proc(hCd, "wdCanvasSector", {P,D,D,D,D,D,D}),
    xwdCanvasChord = define_c_proc(hCd, "wdCanvasChord", {P,D,D,D,D,D,D}),
    xwdCanvasText = define_c_proc(hCd, "wdCanvasText", {P,D,D,P})

global procedure wd_canvas_pixel(atom hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasPixel, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_mark(atom hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasMark, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_line(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasLine, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_vertex(atom hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasVertex, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_rect(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_box(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasBox, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_arc(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_sector(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasSector, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_chord(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasChord, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_text(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw images
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasPutImageRect = define_c_proc(hCd, "wdCanvasImageRect", {P,P,D,D,D,D,D,D}),
    xwdCanvasPutImageRectRGB = define_c_proc(hCd, "wdCanvasImageRectRGB", {P,I,I,P,P,P,D,D,D,D,D,D,D,D,D,D}),
    xwdCanvasPutImageRectRGBA = define_c_proc(hCd, "wdCanvasImageRectRGBA", {P,I,I,P,P,P,P,D,D,D,D,D,D,D,D}),
    xwdCanvasPutImageRectMap = define_c_proc(hCd, "wdCanvasImageRectMap", {P,I,I,P,P,D,D,D,D,D,D,D,D}),
    xwdCanvasPutBitmap = define_c_proc(hCd, "wdCanvasPutBitmap", {P,P,D,D,D,D})

global procedure wd_canvas_put_image_rect(atom hCdCanvas, atom hCdImage, atom x, atom y,
        atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasPutImageRect, {hCdCanvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure

global procedure wd_canvas_put_image_rect_rgb(atom hCdCanvas, atom iw, atom ih,
        sequence rgb, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)
atom pR, pG, pB

    pR = allocate(3*length(rgb[1]))
    pG = pR+length(rgb[1])
    pB = pG+length(rgb[1])
    poke(pR, rgb[1])
    poke(pG, rgb[2])
    poke(pB, rgb[3])
    c_proc(xwdCanvasPutImageRectRGB, {hCdCanvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure wd_canvas_put_image_rect_rgba(atom hCdCanvas, atom iw, atom ih,
        sequence rgba, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pR, pG, pB, pA

    pR = allocate(4*length(rgba[1]))
    pG = pR+length(rgba[1])
    pB = pG+length(rgba[1])
    pA = pB+length(rgba[1])
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xwdCanvasPutImageRectRGBA, {hCdCanvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure wd_canvas_put_image_rect_map(atom hCdCanvas, atom iw, atom ih,
        sequence map, sequence colors,
        atom x, atom y, atom w, atom h,
        atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pIndex, pColors
    pColors = allocate(4*256+length(map))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, map)
    c_proc(xwdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

global procedure wd_canvas_put_bitmap(atom hCdCanvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xwdCanvasPutBitmap, {hCdCanvas, hCdBitmap, x, y, w, h})
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw attributes
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasLineWidth = define_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
    xwdCanvasFont = define_c_func(hCd, "wdCanvasFont", {P,P,I,D},I),
    xwdCanvasGetFont = define_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
    xwdCanvasMarkSize = define_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
    xwdCanvasGetFontDim = define_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
    xwdCanvasGetTextSize = define_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
    xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
    xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
    xwdCanvasStipple = define_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
    xwdCanvasPattern = define_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})

global function wd_canvas_line_width(atom hCdCanvas, atom width)
    return c_func(xwdCanvasLineWidth, {hCdCanvas, width})
end function

global function wd_canvas_font(atom hCdCanvas, sequence font, atom style, atom size)
atom fnVal, pFont

    pFont = allocate_string(font)
    fnVal = c_func(xwdCanvasFont, {hCdCanvas, pFont, style, size})
    free(pFont)
    return fnVal
end function

global function wd_canvas_get_font(atom hCdCanvas)
atom pFont, pSize, pStyle
sequence font
    pStyle = allocate(1024)
    pSize = pStyle+4
    pFont = pSize+8
    c_proc(xwdCanvasGetFont, {hCdCanvas, pFont, pStyle, pSize})
    font = {peek_string(pFont)} & peek4s(pStyle) & peek_double(pSize)
    free(pStyle)
    return font
end function

global function wd_canvas_mark_size(atom hCdCanvas, atom msize)
    return c_func(xwdCanvasMarkSize, {hCdCanvas, msize})
end function

global function wd_canvas_get_font_dim(atom hCdCanvas)
atom pW, pH, pA, pD
sequence font_metrics
    pW = allocate(32)
    pH = pW+8
    pA = pH+8
    pD = pA+8
    c_proc(xwdCanvasGetFontDim, {hCdCanvas, pW, pH, pA, pD})
    font_metrics = peek_double({pW, 4})
    free(pW)
    return font_metrics
end function

global function wd_canvas_get_text_size(atom hCdCanvas, sequence text)
atom pW, pH, pText
sequence text_size

    pW = allocate(16+length(text)+1)
    pH = pW+8
    pText = pH+8
    poke(pText, text & 0)
    c_proc(xwdCanvasGetTextSize, {hCdCanvas, pText, pW, pH})
    text_size = peek_double({pW, 2})
    free(pW)
    return text_size
end function

global function wd_canvas_get_text_box(atom hCdCanvas, atom x, atom y, sequence text)
atom pText, pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(32+length(text)+1)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    pText = pYmax+8
    poke(pText, text & 0)
    c_proc(xwdCanvasGetTextBox, {hCdCanvas, x, y, pText, pXmin, pXmax, pYmin, pYmax})
    box = peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function wd_canvas_get_text_bounds(atom hCdCanvas, atom x, atom y, sequence text)
atom pText, pRect
sequence bounds

    pRect = allocate(64+length(text)+1)
    pText = pRect+64
    poke(pText, text & 0)
    c_proc(xwdCanvasGetTextBounds, {hCdCanvas, x, y, pText, pRect})
    bounds = peek_double({pRect, 8})
    free(pRect)
    return bounds
end function

global procedure wd_canvas_stipple(atom hCdCanvas, atom width, atom height, sequence stipple)
atom pStipple

    pStipple = allocate(length(stipple))
    poke(pStipple, stipple)
    c_proc(xwdCanvasStipple, {hCdCanvas, width, height, pStipple})
    free(pStipple)
end procedure

global procedure wd_canvas_pattern(atom hCdCanvas, atom width, atom height, sequence pattern, atom width_mm, atom height_mm)
atom pPattern

    pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xwdCanvasPattern, {hCdCanvas, width, height, pPattern, width_mm, height_mm})
    free(pPattern)
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw vector text
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasVectorTextDirection = define_c_proc(hCd, "wdCanvasVectorTextDirection", {P,D,D,D,D}),
    xwdCanvasVectorTextSize = define_c_proc(hCd, "wdCanvasVectorTextSize", {P,D,D,P}),
    xwdCanvasGetVectorTextSize = define_c_proc(hCd, "wdCanvasGetVectorTextSize", {P,P,P,P}),
    xwdCanvasVectorCharSize = define_c_func(hCd, "wdCanvasVectorCharSize", {P,D},D),
    xwdCanvasVectorText = define_c_proc(hCd, "wdCanvasVectorText", {P,D,D,P}),
    xwdCanvasMultiLineVectorText = define_c_proc(hCd, "wdCanvasMultiLineVectorText", {P,D,D,P}),
    xwdCanvasGetVectorTextBounds = define_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P})

global procedure wd_canvas_vector_text_direction(atom hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xwdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure wd_canvas_vector_text_size(atom hCdCanvas, atom w, atom h, sequence text)
atom pText

    pText = allocate_string(text)
    c_proc(xwdCanvasVectorTextSize, {hCdCanvas, w, h, pText})
    free(pText)
end procedure

global function wd_canvas_vector_char_size(atom hCdCanvas, atom size)
    return c_func(xwdCanvasVectorCharSize, {hCdCanvas, size})
end function

global function wd_canvas_get_vector_text_size(atom hCdCanvas, sequence text)
atom pText, pX, pY
sequence x_y

    pText = allocate_string(text)
    pX = allocate(16)
    pY = pX+8
    c_proc(xwdCanvasGetVectorTextSize, {hCdCanvas, pText, pX, pY})
    x_y = peek_double({pX, 2})
    free(pText)
    free(pX)
    return x_y
end function

global function wd_canvas_get_vector_text_bounds(atom hCdCanvas, sequence text, atom px, atom py)
atom pRect, pText
sequence rect
    pRect = allocate(8*8)
    pText = allocate_string(text)
    c_proc(xwdCanvasGetVectorTextBounds, {hCdCanvas, pText, px, py, pRect})
    rect = peek_double({pRect, 8})
    free(pRect)
    free(pText)
    return rect
end function

global procedure wd_canvas_vector_text(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

global procedure wd_canvas_multi_line_vector_text(atom hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasMultiLineVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-- pplot.e:
constant hIupPPlot = iup_open_dll({
                                   "iup_pplot.dll",
                                   "libiup_pplot.so",
                                   "libiup_pplot.dylib"
                                  })

--****
-- === Routines

constant
    xIupPPlotOpen = define_c_proc(hIupPPlot, "IupPPlotOpen", {}),
    xIupPPlot = define_c_func(hIupPPlot, "IupPPlot", {},P),
    xIupPPlotBegin = define_c_proc(hIupPPlot, "IupPPlotBegin", {P,I}),
    xIupPPlotAdd = define_c_proc(hIupPPlot, "IupPPlotAdd", {P,F,F}),
    xIupPPlotAddStr = define_c_proc(hIupPPlot, "IupPPlotAddStr", {P,P,F}),
    xIupPPlotEnd = define_c_proc(hIupPPlot, "IupPPlotEnd", {P}),
    xIupPPlotInsert = define_c_proc(hIupPPlot, "IupPPlotInsert", {P,I,I,F,F}),
    xIupPPlotInsertStr = define_c_proc(hIupPPlot, "IupPPlotInsertStr", {P,I,I,P,F}),
    xIupPPlotTransform = define_c_proc(hIupPPlot, "IupPPlotTransform", {P,F,F,P,P}),
    xIupPPlotPaintTo = define_c_proc(hIupPPlot, "IupPPlotPaintTo", {P,P})

integer did_pplot_open = 0

procedure pplot_open()
    did_pplot_open = 1

    c_proc(xIupPPlotOpen, {})
end procedure

global function IupPPlot(sequence attributes = {}, sequence data = {})
    if not did_pplot_open then
        pplot_open()
    end if

    Ihandle ih = c_func(xIupPPlot, {})

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

--**
-- Begin the PPlot
--
-- Note:
--   The name is changed from the Iup name due to harmonization
--   with the "end" routine. End is a key word in Euphoria, thus
--   IupPPlotEnd cannot be shortened to pplot:end as desired. Thus
--   both begin and end have been changed to IupPPlotBegin and end_plot.
--

global procedure IupPPlotBegin(Ihandle ih, integer str_xdata)
    c_proc(xIupPPlotBegin, {ih, str_xdata})
end procedure

global procedure IupPPlotAdd(Ihandle ih, atom x, atom y)
    c_proc(xIupPPlotAdd, {ih, x, y})
end procedure

global procedure add_str(Ihandle ih, sequence x, atom y)
atom pX = allocate_string(x)
    c_proc(xIupPPlotAddStr, {ih, pX, y})
    free(pX)
end procedure

global procedure IupPPlotEnd(Ihandle ih)
    c_proc(xIupPPlotEnd, {ih})
end procedure

global procedure insert_plot(Ihandle ih, integer index, integer sample_index, atom x, atom y)
    c_proc(xIupPPlotInsert, {ih, index, sample_index, x, y})
end procedure

global procedure insert_str(Ihandle ih, integer index, integer sample_index, sequence x, atom y)
atom pX = allocate_string(x)
    c_proc(xIupPPlotInsertStr, {ih, index, sample_index, pX, y})
    free(pX)
end procedure

global function transform_plot(Ihandle ih, atom x, atom y)
atom pX = allocate(4), pY = allocate(4)

    c_proc(xIupPPlotTransform, {ih, x, y, pX, pY})
    x = peek_double(pX)
    y = peek_double(pY)

    free(pX)
    free(pY)

    return {x, y}
end function

global procedure paint_to(Ihandle ih, atom cnv)
    c_proc(xIupPPlotPaintTo, {ih, cnv})
end procedure

--/* (erm, no dll...
--****
-- == OpenGL Canvas
--

constant hIupGL = iup_open_dll({ 
        "iupgl.dll", 
        "libiupgl.so",
        "libiupgl.dylib"
    })

--****
-- === Routines

constant 
    xIupGLOpen = define_c_proc(hIupGL, "IupGLCanvasOpen", {}),
    xIupGLCanvas = define_c_func(hIupGL, "IupGLCanvas", {P},P),
    xIupGLMakeCurrent = define_c_proc(hIupGL, "IupGLMakeCurrent", {P}),
    xIupGLIsCurrent = define_c_func(hIupGL, "IupGLIsCurrent", {P},I),
    xIupGLSwapBuffers = define_c_proc(hIupGL, "IupGLSwapBuffers", {P}),
    xIupGLPalette = define_c_proc(hIupGL, "IupGLPalette", {P,I,F,F,F}),
    xIupGLUseFont = define_c_proc(hIupGL, "IupGLUseFont", {P,I,I,I}),
    xIupGLWait = define_c_proc(hIupGL, "IupGLWait", {I})

integer did_gl_open = 0

procedure gl_open()
    did_gl_open = 1

    c_proc(xIupGLOpen, {})
end procedure

global function IupGLcanvas(sequence action = {}, sequence attributes = {}, sequence data = {})
atom pAction = 0

    if not did_gl_open then
        gl_open()
    end if

    if length(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupGLCanvas, { pAction })

    if pAction!=0 then
        free(pAction)
    end if

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global procedure make_current(Ihandle ih)
    c_proc(xIupGLMakeCurrent, { ih })
end procedure

global function is_current(Ihandle ih)
    return c_func(xIupGLIsCurrent, { ih })
end function

global procedure swap_buffers(Ihandle ih)
    c_proc(xIupGLSwapBuffers, { ih })
end procedure

global procedure palette(Ihandle ih, integer index, atom r, atom g, atom b)
    c_proc(xIupGLPalette, { ih, index, r, g, b })
end procedure

global procedure use_font(Ihandle ih, integer first, integer count, integer list_base)
    c_proc(xIupGLUseFont, { ih, first, count, list_base })
end procedure

global procedure wait(integer gl)
    c_proc(xIupGLWait, { gl })
end procedure

--*/
--DEV
/*
-- imagelib.e:
--****
-- == Image Lib
--

constant hImageLib = iup_open_dll({ 
        "iupimagelib.dll", 
        "libiupimagelib.so", 
        "libiupimagelib.dylib"
    }) 

constant 
--  xIupImageLibOpen = define_c_func(hImageLib, "IupImageLibOpen", {},I)
    xIupImageLibOpen = define_c_proc(hImageLib, "IupImageLibOpen", {})

--****
-- === Routines

global procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure
*/

--ole.e:

--****
-- == OLE Control
--
constant hOle = iup_open_dll({
                              "iupole.dll",
                              "libiupole.so",
                              "libiupole.dylib"
                             })

--****
-- === Routines

constant
        xIupOleControlOpen = define_c_proc(hOle, "IupOleControlOpen", {}),
        xIupOleControl = define_c_func(hOle, "IupOleControl", {P},P)

integer did_ole_open = 0

procedure ole_open()
    did_ole_open = 1
    c_proc(xIupOleControlOpen, {})
end procedure

global function control(sequence prog_id = {}, sequence attributes = {}, sequence data = {})
    if not did_ole_open then
        ole_open()
    end if

    atom pProgId = allocate_string(prog_id)
    Ihandle ih = c_func(xIupOleControl, {pProgId})
    free(pProgId)

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

--/* erm no dll:
--tuio.e:
--****
-- == Tuio Client
--
constant hTuio = iup_open_dll({ 
        "iuptuio.dll", 
        "libiuptuio.so",
        "libiuptuio.dylib"
    })

--****
-- === Routines

constant
    xIupTuioOpen = define_c_proc(hTuio, "IupTuioOpen", {}),
    xIupTuioClient = define_c_func(hTuio, "IupTuioClient", {I},P)

integer did_tuio_open = 0

procedure tuio_open()
    did_tuio_open = 1

    c_proc(xIupTuioOpen, {})
end procedure

global function client(integer port = 3333, sequence attributes = {}, sequence data = {})
    if not did_tuio_open then
        tuio_open()
    end if

    Ihandle ih = c_func(xIupTuioClient, { port })

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function
--*/
