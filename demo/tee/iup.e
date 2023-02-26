namespace iup

--public include std/dll.e
public include std/machine.e

global type Ihandle(integer /*i*/)
    return 1
end type

atom iup = open_dll({"win32\\iup.dll", "libiup.so"})
atom iupimglib = open_dll({"win32\\iupimglib.dll", "libiupimglib.so"})

public constant EXIT_SUCCESS = 0
public constant EXIT_FAILURE = 1

ifdef not EU4_1 then
public function sizeof(atom ctype)
    return and_bits(ctype, #FF)
end function
end ifdef

global function iup_isprint(integer c)
    return c>31 and c<127
end function

-- opposite of allocate_string_pointer_array()
public function peek_string_pointer_array(atom ptr, atom max)
sequence strings = {}
--atom pstr = peek_pointer( ptr )
atom pstr = peek4u(ptr)

    while pstr!=NULL do
        sequence str = peek_string(pstr)
        strings = append(strings, str)
        if length(strings)=max then
            exit
        end if
        ptr += sizeof(C_PTR)
--      pstr = peek_pointer( ptr )
        pstr = peek4u(ptr)
    end while
    return strings
end function

-- allocate an image to memory
public function allocate_image(sequence data, integer cleanup = 0)
atom buff = allocate_data(length(data), cleanup)
    poke(buff, data) return buff
end function

-- return a call_back handle for a routine
public function Icallback(sequence name, atom rid = routine_id(name))
    return call_back({'+', rid})
end function


--public include iupkey.e
/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */
public constant K_SP = ' '   /* 32 (0x20) */
public constant K_exclam = '!' /* 33 */
public constant K_quotedbl = '\"' /* 34 */
public constant K_numbersign = '#' /* 35 */
public constant K_dollar = '$' /* 36 */
public constant K_percent = '%' /* 37 */
public constant K_ampersand = '&' /* 38 */
public constant K_apostrophe = '\'' /* 39 */
public constant K_parentleft = '(' /* 40 */
public constant K_parentright = ')' /* 41 */
public constant K_asterisk = '*' /* 42 */
public constant K_plus = '+' /* 43 */
public constant K_comma = ',' /* 44 */
public constant K_minus = '-' /* 45 */
public constant K_period = '.' /* 46 */
public constant K_slash = '/' /* 47 */
public constant K_0 = '0' /* 48 (0x30) */
public constant K_1 = '1' /* 49 */
public constant K_2 = '2' /* 50 */
public constant K_3 = '3' /* 51 */
public constant K_4 = '4' /* 52 */
public constant K_5 = '5' /* 53 */
public constant K_6 = '6' /* 54 */
public constant K_7 = '7' /* 55 */
public constant K_8 = '8' /* 56 */
public constant K_9 = '9' /* 57 */
public constant K_colon = ':' /* 58 */
public constant K_semicolon = ';' /* 59 */
public constant K_less = '<' /* 60 */
public constant K_equal = '=' /* 61 */
public constant K_greater = '>' /* 62 */
public constant K_question = '?' /* 63 */
public constant K_at = '@' /* 64 */
public constant K_A = 'A' /* 65 (0x41) */
public constant K_B = 'B' /* 66 */
public constant K_C = 'C' /* 67 */
public constant K_D = 'D' /* 68 */
public constant K_E = 'E' /* 69 */
public constant K_F = 'F' /* 70 */
public constant K_G = 'G' /* 71 */
public constant K_H = 'H' /* 72 */
public constant K_I = 'I' /* 73 */
public constant K_J = 'J' /* 74 */
public constant K_K = 'K' /* 75 */
public constant K_L = 'L' /* 76 */
public constant K_M = 'M' /* 77 */
public constant K_N = 'N' /* 78 */
public constant K_O = 'O' /* 79 */
public constant K_P = 'P' /* 80 */
public constant K_Q = 'Q' /* 81 */
public constant K_R = 'R' /* 82 */
public constant K_S = 'S' /* 83 */
public constant K_T = 'T' /* 84 */
public constant K_U = 'U' /* 85 */
public constant K_V = 'V' /* 86 */
public constant K_W = 'W' /* 87 */
public constant K_X = 'X' /* 88 */
public constant K_Y = 'Y' /* 89 */
public constant K_Z = 'Z' /* 90 */
public constant K_bracketleft = '[' /* 91 */
public constant K_backslash = '\\' /* 92 */
public constant K_bracketright = ']' /* 93 */
public constant K_circum = '^' /* 94 */
public constant K_underscore = '_' /* 95 */
public constant K_grave = '`' /* 96 */
public constant K_a = 'a' /* 97 (0x61) */
public constant K_b = 'b' /* 98 */
public constant K_c = 'c' /* 99 */
public constant K_d = 'd' /* 100 */
public constant K_e = 'e' /* 101 */
public constant K_f = 'f' /* 102 */
public constant K_g = 'g' /* 103 */
public constant K_h = 'h' /* 104 */
public constant K_i = 'i' /* 105 */
public constant K_j = 'j' /* 106 */
public constant K_k = 'k' /* 107 */
public constant K_l = 'l' /* 108 */
public constant K_m = 'm' /* 109 */
public constant K_n = 'n' /* 110 */
public constant K_o = 'o' /* 111 */
public constant K_p = 'p' /* 112 */
public constant K_q = 'q' /* 113 */
public constant K_r = 'r' /* 114 */
public constant K_s = 's' /* 115 */
public constant K_t = 't' /* 116 */
public constant K_u = 'u' /* 117 */
public constant K_v = 'v' /* 118 */
public constant K_w = 'w' /* 119 */
public constant K_x = 'x' /* 120 */
public constant K_y = 'y' /* 121 */
public constant K_z = 'z' /* 122 */
public constant K_braceleft = '{' /* 123 */
public constant K_bar = '|' /* 124 */
public constant K_braceright = '}' /* 125 */
public constant K_tilde = '~' /* 126 (0x7E) */
/* Printable ASCii keys */
/* also define the escape sequences that have keys associated */
public constant K_BS = 8 /* 8 */
public constant K_TAB = '\t' /* 9 */
public constant K_LF = '\n' /* 10 (0x0A) not a real key, is a combination of CR with a modifier, just to document */
public constant K_CR = '\r' /* 13 (0x0D) */
/* backward compatible definitions */
public constant K_quoteleft = K_grave
public constant K_quoteright = K_apostrophe
/* IUP Extended Key Codes, range start at 128      */
public constant K_PAUSE = 0xFF13
public constant K_ESC = 0xFF1B
public constant K_HOME = 0xFF50
public constant K_LEFT = 0xFF51
public constant K_UP = 0xFF52
public constant K_RIGHT = 0xFF53
public constant K_DOWN = 0xFF54
public constant K_PGUP = 0xFF55
public constant K_PGDN = 0xFF56
public constant K_END = 0xFF57
public constant K_MIDDLE = 0xFF0B
public constant K_Print = 0xFF61
public constant K_INS = 0xFF63
public constant K_Menu = 0xFF67
public constant K_DEL = 0xFFFF
public constant K_F1 = 0xFFBE
public constant K_F2 = 0xFFBF
public constant K_F3 = 0xFFC0
public constant K_F4 = 0xFFC1
public constant K_F5 = 0xFFC2
public constant K_F6 = 0xFFC3
public constant K_F7 = 0xFFC4
public constant K_F8 = 0xFFC5
public constant K_F9 = 0xFFC6
public constant K_F10 = 0xFFC7
public constant K_F11 = 0xFFC8
public constant K_F12 = 0xFFC9
/* no Shift/Ctrl/Alt */
public constant K_LSHIFT = 0xFFE1
public constant K_RSHIFT = 0xFFE2
public constant K_LCTRL = 0xFFE3
public constant K_RCTRL = 0xFFE4
public constant K_LALT = 0xFFE9
public constant K_RALT = 0xFFEA
public constant K_NUM = 0xFF7F
public constant K_SCROLL = 0xFF14
public constant K_CAPS = 0xFFE5
/* Also, these are the same as the Latin-1 definition */
public constant K_ccedilla = 0x00E7
public constant K_Ccedilla = 0x00C7
public constant K_acute = 0x00B4 /* no Shift/Ctrl/Alt */
public constant K_diaeresis = 0x00A8
/******************************************************/
/* Modifiers use last 4 bits. Since IUP 3.9           */
/* These modifiers definitions are specific to IUP    */
/******************************************************/
public function iup_isShiftXkey( atom _c )
        return and_bits( _c, 0x10000000 )
end function

public function iup_isCtrlXkey( atom _c )
        return and_bits( _c, 0x20000000 )
end function

public function iup_isAltXkey( atom _c )
        return and_bits( _c, 0x40000000 )
end function

public function iup_isSysXkey( atom _c )
        return and_bits( _c, 0x80000000 )
end function

public function iup_XkeyBase( atom _c )
        return and_bits( _c, 0x0FFFFFFF )
end function

public function iup_XkeyShift( atom _c )
        return or_bits( _c, 0x10000000 )
end function

public function iup_XkeyCtrl( atom _c )
        return or_bits( _c, 0x20000000 )
end function

public function iup_XkeyAlt( atom _c )
        return or_bits( _c, 0x40000000 )
end function

public function iup_XkeySys( atom _c )
        return or_bits( _c, 0x80000000 )
end function

public constant K_sHOME = iup_XkeyShift(K_HOME )
public constant K_sUP = iup_XkeyShift(K_UP )
public constant K_sPGUP = iup_XkeyShift(K_PGUP )
public constant K_sLEFT = iup_XkeyShift(K_LEFT )
public constant K_sMIDDLE = iup_XkeyShift(K_MIDDLE )
public constant K_sRIGHT = iup_XkeyShift(K_RIGHT )
public constant K_sEND = iup_XkeyShift(K_END )
public constant K_sDOWN = iup_XkeyShift(K_DOWN )
public constant K_sPGDN = iup_XkeyShift(K_PGDN )
public constant K_sINS = iup_XkeyShift(K_INS )
public constant K_sDEL = iup_XkeyShift(K_DEL )
public constant K_sSP = iup_XkeyShift(K_SP )
public constant K_sTAB = iup_XkeyShift(K_TAB )
public constant K_sCR = iup_XkeyShift(K_CR )
public constant K_sBS = iup_XkeyShift(K_BS )
public constant K_sPAUSE = iup_XkeyShift(K_PAUSE )
public constant K_sESC = iup_XkeyShift(K_ESC )
public constant K_sF1 = iup_XkeyShift(K_F1 )
public constant K_sF2 = iup_XkeyShift(K_F2 )
public constant K_sF3 = iup_XkeyShift(K_F3 )
public constant K_sF4 = iup_XkeyShift(K_F4 )
public constant K_sF5 = iup_XkeyShift(K_F5 )
public constant K_sF6 = iup_XkeyShift(K_F6 )
public constant K_sF7 = iup_XkeyShift(K_F7 )
public constant K_sF8 = iup_XkeyShift(K_F8 )
public constant K_sF9 = iup_XkeyShift(K_F9 )
public constant K_sF10 = iup_XkeyShift(K_F10 )
public constant K_sF11 = iup_XkeyShift(K_F11 )
public constant K_sF12 = iup_XkeyShift(K_F12 )
public constant K_sPrint = iup_XkeyShift(K_Print )
public constant K_sMenu = iup_XkeyShift(K_Menu )
public constant K_cHOME = iup_XkeyCtrl(K_HOME )
public constant K_cUP = iup_XkeyCtrl(K_UP )
public constant K_cPGUP = iup_XkeyCtrl(K_PGUP )
public constant K_cLEFT = iup_XkeyCtrl(K_LEFT )
public constant K_cMIDDLE = iup_XkeyCtrl(K_MIDDLE )
public constant K_cRIGHT = iup_XkeyCtrl(K_RIGHT )
public constant K_cEND = iup_XkeyCtrl(K_END )
public constant K_cDOWN = iup_XkeyCtrl(K_DOWN )
public constant K_cPGDN = iup_XkeyCtrl(K_PGDN )
public constant K_cINS = iup_XkeyCtrl(K_INS )
public constant K_cDEL = iup_XkeyCtrl(K_DEL )
public constant K_cSP = iup_XkeyCtrl(K_SP )
public constant K_cTAB = iup_XkeyCtrl(K_TAB )
public constant K_cCR = iup_XkeyCtrl(K_CR )
public constant K_cBS = iup_XkeyCtrl(K_BS )
public constant K_cPAUSE = iup_XkeyCtrl(K_PAUSE )
public constant K_cESC = iup_XkeyCtrl(K_ESC )
public constant K_cCcedilla = iup_XkeyCtrl(K_Ccedilla)
public constant K_cF1 = iup_XkeyCtrl(K_F1 )
public constant K_cF2 = iup_XkeyCtrl(K_F2 )
public constant K_cF3 = iup_XkeyCtrl(K_F3 )
public constant K_cF4 = iup_XkeyCtrl(K_F4 )
public constant K_cF5 = iup_XkeyCtrl(K_F5 )
public constant K_cF6 = iup_XkeyCtrl(K_F6 )
public constant K_cF7 = iup_XkeyCtrl(K_F7 )
public constant K_cF8 = iup_XkeyCtrl(K_F8 )
public constant K_cF9 = iup_XkeyCtrl(K_F9 )
public constant K_cF10 = iup_XkeyCtrl(K_F10 )
public constant K_cF11 = iup_XkeyCtrl(K_F11 )
public constant K_cF12 = iup_XkeyCtrl(K_F12 )
public constant K_cPrint = iup_XkeyCtrl(K_Print )
public constant K_cMenu = iup_XkeyCtrl(K_Menu )
public constant K_mHOME = iup_XkeyAlt(K_HOME )
public constant K_mUP = iup_XkeyAlt(K_UP )
public constant K_mPGUP = iup_XkeyAlt(K_PGUP )
public constant K_mLEFT = iup_XkeyAlt(K_LEFT )
public constant K_mMIDDLE = iup_XkeyAlt(K_MIDDLE )
public constant K_mRIGHT = iup_XkeyAlt(K_RIGHT )
public constant K_mEND = iup_XkeyAlt(K_END )
public constant K_mDOWN = iup_XkeyAlt(K_DOWN )
public constant K_mPGDN = iup_XkeyAlt(K_PGDN )
public constant K_mINS = iup_XkeyAlt(K_INS )
public constant K_mDEL = iup_XkeyAlt(K_DEL )
public constant K_mSP = iup_XkeyAlt(K_SP )
public constant K_mTAB = iup_XkeyAlt(K_TAB )
public constant K_mCR = iup_XkeyAlt(K_CR )
public constant K_mBS = iup_XkeyAlt(K_BS )
public constant K_mPAUSE = iup_XkeyAlt(K_PAUSE )
public constant K_mESC = iup_XkeyAlt(K_ESC )
public constant K_mCcedilla = iup_XkeyAlt(K_Ccedilla)
public constant K_mF1 = iup_XkeyAlt(K_F1 )
public constant K_mF2 = iup_XkeyAlt(K_F2 )
public constant K_mF3 = iup_XkeyAlt(K_F3 )
public constant K_mF4 = iup_XkeyAlt(K_F4 )
public constant K_mF5 = iup_XkeyAlt(K_F5 )
public constant K_mF6 = iup_XkeyAlt(K_F6 )
public constant K_mF7 = iup_XkeyAlt(K_F7 )
public constant K_mF8 = iup_XkeyAlt(K_F8 )
public constant K_mF9 = iup_XkeyAlt(K_F9 )
public constant K_mF10 = iup_XkeyAlt(K_F10 )
public constant K_mF11 = iup_XkeyAlt(K_F11 )
public constant K_mF12 = iup_XkeyAlt(K_F12 )
public constant K_mPrint = iup_XkeyAlt(K_Print )
public constant K_mMenu = iup_XkeyAlt(K_Menu )
public constant K_yHOME = iup_XkeySys(K_HOME )
public constant K_yUP = iup_XkeySys(K_UP )
public constant K_yPGUP = iup_XkeySys(K_PGUP )
public constant K_yLEFT = iup_XkeySys(K_LEFT )
public constant K_yMIDDLE = iup_XkeySys(K_MIDDLE )
public constant K_yRIGHT = iup_XkeySys(K_RIGHT )
public constant K_yEND = iup_XkeySys(K_END )
public constant K_yDOWN = iup_XkeySys(K_DOWN )
public constant K_yPGDN = iup_XkeySys(K_PGDN )
public constant K_yINS = iup_XkeySys(K_INS )
public constant K_yDEL = iup_XkeySys(K_DEL )
public constant K_ySP = iup_XkeySys(K_SP )
public constant K_yTAB = iup_XkeySys(K_TAB )
public constant K_yCR = iup_XkeySys(K_CR )
public constant K_yBS = iup_XkeySys(K_BS )
public constant K_yPAUSE = iup_XkeySys(K_PAUSE )
public constant K_yESC = iup_XkeySys(K_ESC )
public constant K_yCcedilla = iup_XkeySys(K_Ccedilla)
public constant K_yF1 = iup_XkeySys(K_F1 )
public constant K_yF2 = iup_XkeySys(K_F2 )
public constant K_yF3 = iup_XkeySys(K_F3 )
public constant K_yF4 = iup_XkeySys(K_F4 )
public constant K_yF5 = iup_XkeySys(K_F5 )
public constant K_yF6 = iup_XkeySys(K_F6 )
public constant K_yF7 = iup_XkeySys(K_F7 )
public constant K_yF8 = iup_XkeySys(K_F8 )
public constant K_yF9 = iup_XkeySys(K_F9 )
public constant K_yF10 = iup_XkeySys(K_F10 )
public constant K_yF11 = iup_XkeySys(K_F11 )
public constant K_yF12 = iup_XkeySys(K_F12 )
public constant K_yPrint = iup_XkeySys(K_Print )
public constant K_yMenu = iup_XkeySys(K_Menu )
public constant K_sPlus = iup_XkeyShift(K_plus )
public constant K_sComma = iup_XkeyShift(K_comma )
public constant K_sMinus = iup_XkeyShift(K_minus )
public constant K_sPeriod = iup_XkeyShift(K_period )
public constant K_sSlash = iup_XkeyShift(K_slash )
public constant K_sAsterisk = iup_XkeyShift(K_asterisk)
public constant K_cA = iup_XkeyCtrl(K_A)
public constant K_cB = iup_XkeyCtrl(K_B)
public constant K_cC = iup_XkeyCtrl(K_C)
public constant K_cD = iup_XkeyCtrl(K_D)
public constant K_cE = iup_XkeyCtrl(K_E)
public constant K_cF = iup_XkeyCtrl(K_F)
public constant K_cG = iup_XkeyCtrl(K_G)
public constant K_cH = iup_XkeyCtrl(K_H)
public constant K_cI = iup_XkeyCtrl(K_I)
public constant K_cJ = iup_XkeyCtrl(K_J)
public constant K_cK = iup_XkeyCtrl(K_K)
public constant K_cL = iup_XkeyCtrl(K_L)
public constant K_cM = iup_XkeyCtrl(K_M)
public constant K_cN = iup_XkeyCtrl(K_N)
public constant K_cO = iup_XkeyCtrl(K_O)
public constant K_cP = iup_XkeyCtrl(K_P)
public constant K_cQ = iup_XkeyCtrl(K_Q)
public constant K_cR = iup_XkeyCtrl(K_R)
public constant K_cS = iup_XkeyCtrl(K_S)
public constant K_cT = iup_XkeyCtrl(K_T)
public constant K_cU = iup_XkeyCtrl(K_U)
public constant K_cV = iup_XkeyCtrl(K_V)
public constant K_cW = iup_XkeyCtrl(K_W)
public constant K_cX = iup_XkeyCtrl(K_X)
public constant K_cY = iup_XkeyCtrl(K_Y)
public constant K_cZ = iup_XkeyCtrl(K_Z)
public constant K_c1 = iup_XkeyCtrl(K_1)
public constant K_c2 = iup_XkeyCtrl(K_2)
public constant K_c3 = iup_XkeyCtrl(K_3)
public constant K_c4 = iup_XkeyCtrl(K_4)
public constant K_c5 = iup_XkeyCtrl(K_5)
public constant K_c6 = iup_XkeyCtrl(K_6)
public constant K_c7 = iup_XkeyCtrl(K_7)
public constant K_c8 = iup_XkeyCtrl(K_8)
public constant K_c9 = iup_XkeyCtrl(K_9)
public constant K_c0 = iup_XkeyCtrl(K_0)
public constant K_cPlus = iup_XkeyCtrl(K_plus )
public constant K_cComma = iup_XkeyCtrl(K_comma )
public constant K_cMinus = iup_XkeyCtrl(K_minus )
public constant K_cPeriod = iup_XkeyCtrl(K_period )
public constant K_cSlash = iup_XkeyCtrl(K_slash )
public constant K_cSemicolon = iup_XkeyCtrl(K_semicolon )
public constant K_cEqual = iup_XkeyCtrl(K_equal )
public constant K_cBracketleft = iup_XkeyCtrl(K_bracketleft )
public constant K_cBracketright = iup_XkeyCtrl(K_bracketright)
public constant K_cBackslash = iup_XkeyCtrl(K_backslash )
public constant K_cAsterisk = iup_XkeyCtrl(K_asterisk )
public constant K_mA = iup_XkeyAlt(K_A)
public constant K_mB = iup_XkeyAlt(K_B)
public constant K_mC = iup_XkeyAlt(K_C)
public constant K_mD = iup_XkeyAlt(K_D)
public constant K_mE = iup_XkeyAlt(K_E)
public constant K_mF = iup_XkeyAlt(K_F)
public constant K_mG = iup_XkeyAlt(K_G)
public constant K_mH = iup_XkeyAlt(K_H)
public constant K_mI = iup_XkeyAlt(K_I)
public constant K_mJ = iup_XkeyAlt(K_J)
public constant K_mK = iup_XkeyAlt(K_K)
public constant K_mL = iup_XkeyAlt(K_L)
public constant K_mM = iup_XkeyAlt(K_M)
public constant K_mN = iup_XkeyAlt(K_N)
public constant K_mO = iup_XkeyAlt(K_O)
public constant K_mP = iup_XkeyAlt(K_P)
public constant K_mQ = iup_XkeyAlt(K_Q)
public constant K_mR = iup_XkeyAlt(K_R)
public constant K_mS = iup_XkeyAlt(K_S)
public constant K_mT = iup_XkeyAlt(K_T)
public constant K_mU = iup_XkeyAlt(K_U)
public constant K_mV = iup_XkeyAlt(K_V)
public constant K_mW = iup_XkeyAlt(K_W)
public constant K_mX = iup_XkeyAlt(K_X)
public constant K_mY = iup_XkeyAlt(K_Y)
public constant K_mZ = iup_XkeyAlt(K_Z)
public constant K_m1 = iup_XkeyAlt(K_1)
public constant K_m2 = iup_XkeyAlt(K_2)
public constant K_m3 = iup_XkeyAlt(K_3)
public constant K_m4 = iup_XkeyAlt(K_4)
public constant K_m5 = iup_XkeyAlt(K_5)
public constant K_m6 = iup_XkeyAlt(K_6)
public constant K_m7 = iup_XkeyAlt(K_7)
public constant K_m8 = iup_XkeyAlt(K_8)
public constant K_m9 = iup_XkeyAlt(K_9)
public constant K_m0 = iup_XkeyAlt(K_0)
public constant K_mPlus = iup_XkeyAlt(K_plus )
public constant K_mComma = iup_XkeyAlt(K_comma )
public constant K_mMinus = iup_XkeyAlt(K_minus )
public constant K_mPeriod = iup_XkeyAlt(K_period )
public constant K_mSlash = iup_XkeyAlt(K_slash )
public constant K_mSemicolon = iup_XkeyAlt(K_semicolon )
public constant K_mEqual = iup_XkeyAlt(K_equal )
public constant K_mBracketleft = iup_XkeyAlt(K_bracketleft )
public constant K_mBracketright = iup_XkeyAlt(K_bracketright)
public constant K_mBackslash = iup_XkeyAlt(K_backslash )
public constant K_mAsterisk = iup_XkeyAlt(K_asterisk )
public constant K_yA = iup_XkeySys(K_A)
public constant K_yB = iup_XkeySys(K_B)
public constant K_yC = iup_XkeySys(K_C)
public constant K_yD = iup_XkeySys(K_D)
public constant K_yE = iup_XkeySys(K_E)
public constant K_yF = iup_XkeySys(K_F)
public constant K_yG = iup_XkeySys(K_G)
public constant K_yH = iup_XkeySys(K_H)
public constant K_yI = iup_XkeySys(K_I)
public constant K_yJ = iup_XkeySys(K_J)
public constant K_yK = iup_XkeySys(K_K)
public constant K_yL = iup_XkeySys(K_L)
public constant K_yM = iup_XkeySys(K_M)
public constant K_yN = iup_XkeySys(K_N)
public constant K_yO = iup_XkeySys(K_O)
public constant K_yP = iup_XkeySys(K_P)
public constant K_yQ = iup_XkeySys(K_Q)
public constant K_yR = iup_XkeySys(K_R)
public constant K_yS = iup_XkeySys(K_S)
public constant K_yT = iup_XkeySys(K_T)
public constant K_yU = iup_XkeySys(K_U)
public constant K_yV = iup_XkeySys(K_V)
public constant K_yW = iup_XkeySys(K_W)
public constant K_yX = iup_XkeySys(K_X)
public constant K_yY = iup_XkeySys(K_Y)
public constant K_yZ = iup_XkeySys(K_Z)
public constant K_y1 = iup_XkeySys(K_1)
public constant K_y2 = iup_XkeySys(K_2)
public constant K_y3 = iup_XkeySys(K_3)
public constant K_y4 = iup_XkeySys(K_4)
public constant K_y5 = iup_XkeySys(K_5)
public constant K_y6 = iup_XkeySys(K_6)
public constant K_y7 = iup_XkeySys(K_7)
public constant K_y8 = iup_XkeySys(K_8)
public constant K_y9 = iup_XkeySys(K_9)
public constant K_y0 = iup_XkeySys(K_0)
public constant K_yPlus = iup_XkeySys(K_plus )
public constant K_yComma = iup_XkeySys(K_comma )
public constant K_yMinus = iup_XkeySys(K_minus )
public constant K_yPeriod = iup_XkeySys(K_period )
public constant K_ySlash = iup_XkeySys(K_slash )
public constant K_ySemicolon = iup_XkeySys(K_semicolon )
public constant K_yEqual = iup_XkeySys(K_equal )
public constant K_yBracketleft = iup_XkeySys(K_bracketleft )
public constant K_yBracketright = iup_XkeySys(K_bracketright)
public constant K_yBackslash = iup_XkeySys(K_backslash )
public constant K_yAsterisk = iup_XkeySys(K_asterisk )

--public include iupdef.e
/* Deprecated definitions */
/* Avoid using these definitions. Use the strings instead. */
/* Define __IUPDEF_H to avoid the inclusion of this header */
public constant IUP_RUN = "RUN"
public constant IUP_ENGLISH = "ENGLISH"
public constant IUP_PORTUGUESE = "PORTUGUESE"
public constant IUP_SBH = "SBH"
public constant IUP_SBV = "SBV"
/************************************************************************/
/*                            Callbacks                                 */
/************************************************************************/
public constant IUP_DEFAULT_ACTION = "DEFAULT_ACTION"
public constant IUP_IDLE_ACTION = "IDLE_ACTION"
public constant IUP_ACTION = "ACTION"
public constant IUP_GETFOCUS_CB = "GETFOCUS_CB"
public constant IUP_KILLFOCUS_CB = "KILLFOCUS_CB"
public constant IUP_K_ANY = "K_ANY"
public constant IUP_KEYPRESS_CB = "KEYPRESS_CB"
public constant IUP_HELP_CB = "HELP_CB"
public constant IUP_SCROLL_CB = "SCROLL_CB"
public constant IUP_RESIZE_CB = "RESIZE_CB"
public constant IUP_MOTION_CB = "MOTION_CB"
public constant IUP_BUTTON_CB = "BUTTON_CB"
public constant IUP_ENTERWINDOW_CB = "ENTERWINDOW_CB"
public constant IUP_LEAVEWINDOW_CB = "LEAVEWINDOW_CB"
public constant IUP_WHEEL_CB = "WHEEL_CB"
public constant IUP_MASK_CB = "MASK_CB"
public constant IUP_OPEN_CB = "OPEN_CB"
public constant IUP_HIGHLIGHT_CB = "HIGHLIGHT_CB"
public constant IUP_MENUCLOSE_CB = "MENUCLOSE_CB"
public constant IUP_MAP_CB = "MAP_CB"
public constant IUP_CLOSE_CB = "CLOSE_CB"
public constant IUP_SHOW_CB = "SHOW_CB"
public constant IUP_DROPFILES_CB = "DROPFILES_CB"
public constant IUP_WOM_CB = "WOM_CB"
/************************************************************************/
/*                            Attributes                                */
/************************************************************************/
public constant IUP_DIRECTION = "DIRECTION"
public constant IUP_ACTIVE = "ACTIVE"
public constant IUP_BGCOLOR = "BGCOLOR"
public constant IUP_FRAMECOLOR = "FRAMECOLOR"
public constant IUP_FGCOLOR = "FGCOLOR"
public constant IUP_COLOR = "COLOR"
public constant IUP_WID = "WID"
public constant IUP_SIZE = "SIZE"
public constant IUP_RASTERSIZE = "RASTERSIZE"
public constant IUP_TITLE = "TITLE"
public constant IUP_VALUE = "VALUE"
public constant IUP_VISIBLE = "VISIBLE"
public constant IUP_FONT = "FONT"
public constant IUP_TIP = "TIP"
public constant IUP_EXPAND = "EXPAND"
public constant IUP_SEPARATOR = "SEPARATOR"
public constant IUP_HOTSPOT = "HOTSPOT"
public constant IUP_HEIGHT = "HEIGHT"
public constant IUP_WIDTH = "WIDTH"
public constant IUP_KEY = "KEY"
public constant IUP_MULTIPLE = "MULTIPLE"
public constant IUP_DROPDOWN = "DROPDOWN"
public constant IUP_VISIBLE_ITEMS = "VISIBLE_ITEMS"
public constant IUP_MARGIN = "MARGIN"
public constant IUP_GAP = "GAP"
public constant IUP_ALIGNMENT = "ALIGNMENT"
public constant IUP_IMAGE = "IMAGE"
public constant IUP_IMINACTIVE = "IMINACTIVE"
public constant IUP_IMPRESS = "IMPRESS"
public constant IUP_WIN_SAVEBITS = "WIN_SAVEBITS"
public constant IUP_NC = "NC"
public constant IUP_MASK = "MASK"
public constant IUP_APPEND = "APPEND"
public constant IUP_BORDER = "BORDER"
public constant IUP_CARET = "CARET"
public constant IUP_SELECTION = "SELECTION"
public constant IUP_SELECTEDTEXT = "SELECTEDTEXT"
public constant IUP_INSERT = "INSERT"
public constant IUP_CONID = "CONID"
public constant IUP_CURSOR = "CURSOR"
public constant IUP_ICON = "ICON"
public constant IUP_MENUBOX = "MENUBOX"
public constant IUP_MINBOX = "MINBOX"
public constant IUP_MAXBOX = "MAXBOX"
public constant IUP_RESIZE = "RESIZE"
public constant IUP_MENU = "MENU"
public constant IUP_STARTFOCUS = "STARTFOCUS"
public constant IUP_PARENTDIALOG = "PARENTDIALOG"
public constant IUP_SHRINK = "SHRINK"
public constant IUP_DEFAULTENTER = "DEFAULTENTER"
public constant IUP_DEFAULTESC = "DEFAULTESC"
public constant IUP_X = "X"
public constant IUP_Y = "Y"
public constant IUP_TOOLBOX = "TOOLBOX"
public constant IUP_CONTROL = "CONTROL"
public constant IUP_READONLY = "READONLY"
public constant IUP_SCROLLBAR = "SCROLLBAR"
public constant IUP_POSY = "POSY"
public constant IUP_POSX = "POSX"
public constant IUP_DX = "DX"
public constant IUP_DY = "DY"
public constant IUP_XMAX = "XMAX"
public constant IUP_XMIN = "XMIN"
public constant IUP_YMAX = "YMAX"
public constant IUP_YMIN = "YMIN"
public constant IUP_RED = "255 0 0"
public constant IUP_GREEN = "0 255 0"
public constant IUP_BLUE = "0 0 255"
public constant IUP_MIN = "MIN"
public constant IUP_MAX = "MAX"
public constant IUP_TIME = "TIME"
public constant IUP_DRAG = "DRAG"
public constant IUP_DROP = "DROP"
public constant IUP_REPAINT = "REPAINT"
public constant IUP_TOPMOST = "TOPMOST"
public constant IUP_CLIPCHILDREN = "CLIPCHILDREN"
public constant IUP_DIALOGTYPE = "DIALOGTYPE"
public constant IUP_FILE = "FILE"
public constant IUP_MULTIPLEFILES = "MULTIPLEFILES"
public constant IUP_FILTER = "FILTER"
public constant IUP_FILTERUSED = "FILTERUSED"
public constant IUP_FILTERINFO = "FILTERINFO"
public constant IUP_EXTFILTER = "EXTFILTER"
public constant IUP_DIRECTORY = "DIRECTORY"
public constant IUP_ALLOWNEW = "ALLOWNEW"
public constant IUP_NOOVERWRITEPROMPT = "NOOVERWRITEPROMPT"
public constant IUP_NOCHANGEDIR = "NOCHANGEDIR"
public constant IUP_FILEEXIST = "FILEEXIST"
public constant IUP_STATUS = "STATUS"
public constant IUP_LOCKLOOP = "LOCKLOOP"
public constant IUP_SYSTEM = "SYSTEM"
public constant IUP_DRIVER = "DRIVER"
public constant IUP_SCREENSIZE = "SCREENSIZE"
public constant IUP_SYSTEMLANGUAGE = "SYSTEMLANGUAGE"
public constant IUP_COMPUTERNAME = "COMPUTERNAME"
public constant IUP_USERNAME = "USERNAME"
public constant IUP_OPEN = "OPEN"
public constant IUP_SAVE = "SAVE"
public constant IUP_DIR = "DIR"
public constant IUP_HORIZONTAL = "HORIZONTAL"
public constant IUP_VERTICAL = "VERTICAL"
/************************************************************************/
/*                       Attribute Values                               */
/************************************************************************/
public constant IUP_YES = "YES"
public constant IUP_NO = "NO"
public constant IUP_ON = "ON"
public constant IUP_OFF = "OFF"
public constant IUP_ACENTER = "ACENTER"
public constant IUP_ALEFT = "ALEFT"
public constant IUP_ARIGHT = "ARIGHT"
public constant IUP_ATOP = "ATOP"
public constant IUP_ABOTTOM = "ABOTTOM"
public constant IUP_NORTH = "NORTH"
public constant IUP_SOUTH = "SOUTH"
public constant IUP_WEST = "WEST"
public constant IUP_EAST = "EAST"
public constant IUP_NE = "NE"
public constant IUP_SE = "SE"
public constant IUP_NW = "NW"
public constant IUP_SW = "SW"
public constant IUP_FULLSCREEN = "FULLSCREEN"
public constant IUP_FULL = "FULL"
public constant IUP_HALF = "HALF"
public constant IUP_THIRD = "THIRD"
public constant IUP_QUARTER = "QUARTER"
public constant IUP_EIGHTH = "EIGHTH"
public constant IUP_ARROW = "ARROW"
public constant IUP_BUSY = "BUSY"
public constant IUP_RESIZE_N = "RESIZE_N"
public constant IUP_RESIZE_S = "RESIZE_S"
public constant IUP_RESIZE_E = "RESIZE_E"
public constant IUP_RESIZE_W = "RESIZE_W"
public constant IUP_RESIZE_NE = "RESIZE_NE"
public constant IUP_RESIZE_NW = "RESIZE_NW"
public constant IUP_RESIZE_SE = "RESIZE_SE"
public constant IUP_RESIZE_SW = "RESIZE_SW"
public constant IUP_MOVE = "MOVE"
public constant IUP_HAND = "HAND"
public constant IUP_NONE = "NONE"
public constant IUP_IUP = "IUP"
public constant IUP_CROSS = "CROSS"
public constant IUP_PEN = "PEN"
public constant IUP_TEXT = "TEXT"
public constant IUP_RESIZE_C = "RESIZE_C"
public constant IUP_OPENHAND = "OPENHAND"
/*****************/
/* Fonts        */
/*****************/
public constant IUP_HELVETICA_NORMAL_8 = "HELVETICA_NORMAL_8"
public constant IUP_HELVETICA_ITALIC_8 = "HELVETICA_ITALIC_8"
public constant IUP_HELVETICA_BOLD_8 = "HELVETICA_BOLD_8"
public constant IUP_HELVETICA_NORMAL_10 = "HELVETICA_NORMAL_10"
public constant IUP_HELVETICA_ITALIC_10 = "HELVETICA_ITALIC_10"
public constant IUP_HELVETICA_BOLD_10 = "HELVETICA_BOLD_10"
public constant IUP_HELVETICA_NORMAL_12 = "HELVETICA_NORMAL_12"
public constant IUP_HELVETICA_ITALIC_12 = "HELVETICA_ITALIC_12"
public constant IUP_HELVETICA_BOLD_12 = "HELVETICA_BOLD_12"
public constant IUP_HELVETICA_NORMAL_14 = "HELVETICA_NORMAL_14"
public constant IUP_HELVETICA_ITALIC_14 = "HELVETICA_ITALIC_14"
public constant IUP_HELVETICA_BOLD_14 = "HELVETICA_BOLD_14"
public constant IUP_COURIER_NORMAL_8 = "COURIER_NORMAL_8"
public constant IUP_COURIER_ITALIC_8 = "COURIER_ITALIC_8"
public constant IUP_COURIER_BOLD_8 = "COURIER_BOLD_8"
public constant IUP_COURIER_NORMAL_10 = "COURIER_NORMAL_10"
public constant IUP_COURIER_ITALIC_10 = "COURIER_ITALIC_10"
public constant IUP_COURIER_BOLD_10 = "COURIER_BOLD_10"
public constant IUP_COURIER_NORMAL_12 = "COURIER_NORMAL_12"
public constant IUP_COURIER_ITALIC_12 = "COURIER_ITALIC_12"
public constant IUP_COURIER_BOLD_12 = "COURIER_BOLD_12"
public constant IUP_COURIER_NORMAL_14 = "COURIER_NORMAL_14"
public constant IUP_COURIER_ITALIC_14 = "COURIER_ITALIC_14"
public constant IUP_COURIER_BOLD_14 = "COURIER_BOLD_14"
public constant IUP_TIMES_NORMAL_8 = "TIMES_NORMAL_8"
public constant IUP_TIMES_ITALIC_8 = "TIMES_ITALIC_8"
public constant IUP_TIMES_BOLD_8 = "TIMES_BOLD_8"
public constant IUP_TIMES_NORMAL_10 = "TIMES_NORMAL_10"
public constant IUP_TIMES_ITALIC_10 = "TIMES_ITALIC_10"
public constant IUP_TIMES_BOLD_10 = "TIMES_BOLD_10"
public constant IUP_TIMES_NORMAL_12 = "TIMES_NORMAL_12"
public constant IUP_TIMES_ITALIC_12 = "TIMES_ITALIC_12"
public constant IUP_TIMES_BOLD_12 = "TIMES_BOLD_12"
public constant IUP_TIMES_NORMAL_14 = "TIMES_NORMAL_14"
public constant IUP_TIMES_ITALIC_14 = "TIMES_ITALIC_14"
public constant IUP_TIMES_BOLD_14 = "TIMES_BOLD_14"
/************************************************************************/
/*                           Keys                                       */
/************************************************************************/
public constant IUP_K_exclam = "K_exclam"
public constant IUP_K_quotedbl = "K_quotedbl"
public constant IUP_K_numbersign = "K_numbersign"
public constant IUP_K_dollar = "K_dollar"
public constant IUP_K_percent = "K_percent"
public constant IUP_K_ampersand = "K_ampersand"
public constant IUP_K_quoteright = "K_quoteright"
public constant IUP_K_parentleft = "K_parentleft"
public constant IUP_K_parentright = "K_parentright"
public constant IUP_K_asterisk = "K_asterisk"
public constant IUP_K_plus = "K_plus"
public constant IUP_K_comma = "K_comma"
public constant IUP_K_minus = "K_minus"
public constant IUP_K_period = "K_period"
public constant IUP_K_slash = "K_slash"
public constant IUP_K_0 = "K_0"
public constant IUP_K_1 = "K_1"
public constant IUP_K_2 = "K_2"
public constant IUP_K_3 = "K_3"
public constant IUP_K_4 = "K_4"
public constant IUP_K_5 = "K_5"
public constant IUP_K_6 = "K_6"
public constant IUP_K_7 = "K_7"
public constant IUP_K_8 = "K_8"
public constant IUP_K_9 = "K_9"
public constant IUP_K_colon = "K_colon"
public constant IUP_K_semicolon = "K_semicolon "
public constant IUP_K_less = "K_less"
public constant IUP_K_equal = "K_equal"
public constant IUP_K_greater = "K_greater"
public constant IUP_K_question = "K_question"
public constant IUP_K_at = "K_at"
public constant IUP_K_A = "K_A"
public constant IUP_K_B = "K_B"
public constant IUP_K_C = "K_C"
public constant IUP_K_D = "K_D"
public constant IUP_K_E = "K_E"
public constant IUP_K_F = "K_F"
public constant IUP_K_G = "K_G"
public constant IUP_K_H = "K_H"
public constant IUP_K_I = "K_I"
public constant IUP_K_J = "K_J"
public constant IUP_K_K = "K_K"
public constant IUP_K_L = "K_L"
public constant IUP_K_M = "K_M"
public constant IUP_K_N = "K_N"
public constant IUP_K_O = "K_O"
public constant IUP_K_P = "K_P"
public constant IUP_K_Q = "K_Q"
public constant IUP_K_R = "K_R"
public constant IUP_K_S = "K_S"
public constant IUP_K_T = "K_T"
public constant IUP_K_U = "K_U"
public constant IUP_K_V = "K_V"
public constant IUP_K_W = "K_W"
public constant IUP_K_X = "K_X"
public constant IUP_K_Y = "K_Y"
public constant IUP_K_Z = "K_Z"
public constant IUP_K_bracketleft = "K_bracketleft"
public constant IUP_K_backslash = "K_backslash"
public constant IUP_K_bracketright = "K_bracketright"
public constant IUP_K_circum = "K_circum"
public constant IUP_K_underscore = "K_underscore"
public constant IUP_K_quoteleft = "K_quoteleft"
public constant IUP_K_a = "K_a"
public constant IUP_K_b = "K_b"
public constant IUP_K_c = "K_c"
public constant IUP_K_d = "K_d"
public constant IUP_K_e = "K_e"
public constant IUP_K_f = "K_f"
public constant IUP_K_g = "K_g"
public constant IUP_K_h = "K_h"
public constant IUP_K_i = "K_i"
public constant IUP_K_j = "K_j"
public constant IUP_K_k = "K_k"
public constant IUP_K_l = "K_l"
public constant IUP_K_m = "K_m"
public constant IUP_K_n = "K_n"
public constant IUP_K_o = "K_o"
public constant IUP_K_p = "K_p"
public constant IUP_K_q = "K_q"
public constant IUP_K_r = "K_r"
public constant IUP_K_s = "K_s"
public constant IUP_K_t = "K_t"
public constant IUP_K_u = "K_u"
public constant IUP_K_v = "K_v"
public constant IUP_K_w = "K_w"
public constant IUP_K_x = "K_x"
public constant IUP_K_y = "K_y"
public constant IUP_K_z = "K_z"
public constant IUP_K_braceleft = "K_braceleft"
public constant IUP_K_bar = "K_bar"
public constant IUP_K_braceright = "K_braceright"
public constant IUP_K_tilde = "K_tilde"
public constant IUP_K_cA = "K_cA"
public constant IUP_K_cB = "K_cB"
public constant IUP_K_cC = "K_cC"
public constant IUP_K_cD = "K_cD"
public constant IUP_K_cE = "K_cE"
public constant IUP_K_cF = "K_cF"
public constant IUP_K_cG = "K_cG"
public constant IUP_K_cJ = "K_cJ"
public constant IUP_K_cK = "K_cK"
public constant IUP_K_cL = "K_cL"
public constant IUP_K_cN = "K_cN"
public constant IUP_K_cO = "K_cO"
public constant IUP_K_cP = "K_cP"
public constant IUP_K_cQ = "K_cQ"
public constant IUP_K_cR = "K_cR"
public constant IUP_K_cS = "K_cS"
public constant IUP_K_cT = "K_cT"
public constant IUP_K_cU = "K_cU"
public constant IUP_K_cV = "K_cV"
public constant IUP_K_cW = "K_cW"
public constant IUP_K_cX = "K_cX"
public constant IUP_K_cY = "K_cY"
public constant IUP_K_cZ = "K_cZ"
public constant IUP_K_mA = "K_mA"
public constant IUP_K_mB = "K_mB"
public constant IUP_K_mC = "K_mC"
public constant IUP_K_mD = "K_mD"
public constant IUP_K_mE = "K_mE"
public constant IUP_K_mF = "K_mF"
public constant IUP_K_mG = "K_mG"
public constant IUP_K_mH = "K_mH"
public constant IUP_K_mI = "K_mI"
public constant IUP_K_mJ = "K_mJ"
public constant IUP_K_mK = "K_mK"
public constant IUP_K_mL = "K_mL"
public constant IUP_K_mM = "K_mM"
public constant IUP_K_mN = "K_mN"
public constant IUP_K_mO = "K_mO"
public constant IUP_K_mP = "K_mP"
public constant IUP_K_mQ = "K_mQ"
public constant IUP_K_mR = "K_mR"
public constant IUP_K_mS = "K_mS"
public constant IUP_K_mT = "K_mT"
public constant IUP_K_mU = "K_mU"
public constant IUP_K_mV = "K_mV"
public constant IUP_K_mW = "K_mW"
public constant IUP_K_mX = "K_mX"
public constant IUP_K_mY = "K_mY"
public constant IUP_K_mZ = "K_mZ"
public constant IUP_K_BS = "K_BS"
public constant IUP_K_TAB = "K_TAB"
public constant IUP_K_CR = "K_CR"
public constant IUP_K_SP = "K_SP"
public constant IUP_K_ESC = "K_ESC"
public constant IUP_K_sCR = "K_sCR"
public constant IUP_K_sTAB = "K_sTAB"
public constant IUP_K_cTAB = "K_cTAB"
public constant IUP_K_mTAB = "K_mTAB"
public constant IUP_K_HOME = "K_HOME"
public constant IUP_K_UP = "K_UP"
public constant IUP_K_PGUP = "K_PGUP"
public constant IUP_K_LEFT = "K_LEFT"
public constant IUP_K_RIGHT = "K_RIGHT"
public constant IUP_K_END = "K_END"
public constant IUP_K_DOWN = "K_DOWN"
public constant IUP_K_PGDN = "K_PGDN"
public constant IUP_K_MIDDLE = "K_MIDDLE"
public constant IUP_K_INS = "K_INS"
public constant IUP_K_DEL = "K_DEL"
public constant IUP_K_sHOME = "K_sHOME"
public constant IUP_K_sUP = "K_sUP"
public constant IUP_K_sPGUP = "K_sPGUP"
public constant IUP_K_sLEFT = "K_sLEFT"
public constant IUP_K_sRIGHT = "K_sRIGHT"
public constant IUP_K_sEND = "K_sEND"
public constant IUP_K_sDOWN = "K_sDOWN"
public constant IUP_K_sPGDN = "K_sPGDN"
public constant IUP_K_cHOME = "K_cHOME"
public constant IUP_K_cPGUP = "K_cPGUP"
public constant IUP_K_cLEFT = "K_cLEFT"
public constant IUP_K_cRIGHT = "K_cRIGHT"
public constant IUP_K_cEND = "K_cEND"
public constant IUP_K_cPGDN = "K_cPGDN"
public constant IUP_K_cUP = "K_cUP"
public constant IUP_K_cDOWN = "K_cDOWN"
public constant IUP_K_cMIDDLE = "K_cMIDDLE"
public constant IUP_K_cINS = "K_cINS"
public constant IUP_K_cDEL = "K_cDEL"
public constant IUP_K_mHOME = "K_mHOME"
public constant IUP_K_mPGUP = "K_mPGUP"
public constant IUP_K_mLEFT = "K_mLEFT"
public constant IUP_K_mRIGHT = "K_mRIGHT"
public constant IUP_K_mEND = "K_mEND"
public constant IUP_K_mPGDN = "K_mPGDN"
public constant IUP_K_mUP = "K_mUP"
public constant IUP_K_mDOWN = "K_mDOWN"
public constant IUP_K_mINS = "K_mINS"
public constant IUP_K_mDEL = "K_mDEL"
public constant IUP_K_F1 = "K_F1"
public constant IUP_K_F2 = "K_F2"
public constant IUP_K_F3 = "K_F3"
public constant IUP_K_F4 = "K_F4"
public constant IUP_K_F5 = "K_F5"
public constant IUP_K_F6 = "K_F6"
public constant IUP_K_F7 = "K_F7"
public constant IUP_K_F8 = "K_F8"
public constant IUP_K_F9 = "K_F9"
public constant IUP_K_F10 = "K_F10"
public constant IUP_K_F11 = "K_F11"
public constant IUP_K_F12 = "K_F12"
public constant IUP_K_sF1 = "K_sF1"
public constant IUP_K_sF2 = "K_sF2"
public constant IUP_K_sF3 = "K_sF3"
public constant IUP_K_sF4 = "K_sF4"
public constant IUP_K_sF5 = "K_sF5"
public constant IUP_K_sF6 = "K_sF6"
public constant IUP_K_sF7 = "K_sF7"
public constant IUP_K_sF8 = "K_sF8"
public constant IUP_K_sF9 = "K_sF9"
public constant IUP_K_sF10 = "K_sF10"
public constant IUP_K_sF11 = "K_sF11"
public constant IUP_K_sF12 = "K_sF12"
public constant IUP_K_cF1 = "K_cF1"
public constant IUP_K_cF2 = "K_cF2"
public constant IUP_K_cF3 = "K_cF3"
public constant IUP_K_cF4 = "K_cF4"
public constant IUP_K_cF5 = "K_cF5"
public constant IUP_K_cF6 = "K_cF6"
public constant IUP_K_cF7 = "K_cF7"
public constant IUP_K_cF8 = "K_cF8"
public constant IUP_K_cF9 = "K_cF9"
public constant IUP_K_cF10 = "K_cF10"
public constant IUP_K_cF11 = "K_cF11"
public constant IUP_K_cF12 = "K_cF12"
public constant IUP_K_mF1 = "K_mF1"
public constant IUP_K_mF2 = "K_mF2"
public constant IUP_K_mF3 = "K_mF3"
public constant IUP_K_mF4 = "K_mF4"
public constant IUP_K_mF5 = "K_mF5"
public constant IUP_K_mF6 = "K_mF6"
public constant IUP_K_mF7 = "K_mF7"
public constant IUP_K_mF8 = "K_mF8"
public constant IUP_K_mF9 = "K_mF9"
public constant IUP_K_mF10 = "K_mF10"
public constant IUP_K_m1 = "K_m1"
public constant IUP_K_m2 = "K_m2"
public constant IUP_K_m3 = "K_m3"
public constant IUP_K_m4 = "K_m4"
public constant IUP_K_m5 = "K_m5"
public constant IUP_K_m6 = "K_m6"
public constant IUP_K_m7 = "K_m7"
public constant IUP_K_m8 = "K_m8"
public constant IUP_K_m9 = "K_m9"
public constant IUP_K_m0 = "K_m0"
/************/
/* Colorbar */
/************/
public constant IUP_NUM_PARTS = "NUM_PARTS"
public constant IUP_NUM_CELLS = "NUM_CELLS"
public constant IUP_CELL = "CELL"
public constant IUP_PREVIEW_SIZE = "PREVIEW_SIZE"
public constant IUP_SHOW_PREVIEW = "SHOW_PREVIEW"
public constant IUP_SHOW_SECONDARY = "SHOW_SECONDARY"
public constant IUP_PRIMARY_CELL = "PRIMARY_CELL"
public constant IUP_SECONDARY_CELL = "SECONDARY_CELL"
public constant IUP_ORIENTATION = "ORIENTATION"
public constant IUP_SQUARED = "SQUARED"
public constant IUP_SHADOWED = "SHADOWED"
public constant IUP_BUFFERIZE = "BUFFERIZE"
public constant IUP_TRANSPARENCY = "TRANSPARENCY"
public constant IUP_CELL_CB = "CELL_CB"
public constant IUP_EXTENDED_CB = "EXTENDED_CB"
public constant IUP_SELECT_CB = "SELECT_CB"
public constant IUP_SWITCH_CB = "SWITCH_CB"
--public constant IUP_VERTICAL = "VERTICAL"
--public constant IUP_HORIZONTAL = "HORIZONTAL"
/************/
/* Cells    */
/************/
public constant IUP_ALL = "ALL"
public constant IUP_BOXED = "BOXED"
public constant IUP_CLIPPED = "CLIPPED"
public constant IUP_TRANSPARENT = "TRANSPARENT"
public constant IUP_NON_SCROLLABLE_LINES = "NON_SCROLLABLE_LINES"
public constant IUP_NON_SCROLLABLE_COLS = "NON_SCROLLABLE_COLS"
public constant IUP_ORIGIN = "ORIGIN"
public constant IUP_NO_COLOR = "NO_COLOR"
public constant IUP_FIRST_LINE = "FIRST_LINE"
public constant IUP_FIRST_COL = "FIRST_COL"
public constant IUP_DOUBLE_BUFFER = "DOUBLE_BUFFER"
public constant IUP_LIMITS = "LIMITS"
public constant IUP_CANVAS = "CANVAS"
public constant IUP_IMAGE_CANVAS = "IMAGE_CANVAS"
public constant IUP_FULL_VISIBLE = "FULL_VISIBLE"
public constant IUP_MOUSECLICK_CB = "MOUSECLICK_CB"
public constant IUP_MOUSEMOTION_CB = "MOUSEMOTION_CB"
public constant IUP_DRAW_CB = "DRAW_CB"
public constant IUP_WIDTH_CB = "WIDTH_CB"
public constant IUP_HEIGHT_CB = "HEIGHT_CB"
public constant IUP_NLINES_CB = "NLINES_CB"
public constant IUP_NCOLS_CB = "NCOLS_CB"
public constant IUP_HSPAN_CB = "HSPAN_CB"
public constant IUP_VSPAN_CB = "VSPAN_CB"
public constant IUP_SCROLLING_CB = "SCROLLING_CB"
/*****************/
/* ColorBrowser  */
/*****************/
public constant IUP_RGB = "RGB"
public constant IUP_CHANGE_CB = "CHANGE_CB"
public constant IUP_DRAG_CB = "DRAG_CB"
/*****************/
/* Val           */
/*****************/
public constant ICTL_MOUSEMOVE_CB = "MOUSEMOVE_CB"
public constant ICTL_BUTTON_PRESS_CB = "BUTTON_PRESS_CB"
public constant ICTL_BUTTON_RELEASE_CB = "BUTTON_RELEASE_CB"
public constant ICTL_HORIZONTAL = "HORIZONTAL"
public constant ICTL_VERTICAL = "VERTICAL"
public constant ICTL_SHOWTICKS = "SHOWTICKS"
/*****************/
/* Tabs          */
/*****************/
public constant ICTL_TOP = "TOP"
public constant ICTL_BOTTOM = "BOTTOM"
public constant ICTL_LEFT = "LEFT"
public constant ICTL_RIGHT = "RIGHT"
public constant ICTL_TABTYPE = "TABTYPE"
public constant ICTL_TABTITLE = "TABTITLE"
public constant ICTL_TABSIZE = "TABSIZE"
public constant ICTL_TABCHANGE_CB = "TABCHANGE_CB"
public constant ICTL_FONT = "FONT"
public constant ICTL_FONT_ACTIVE = "FONT_ACTIVE"
public constant ICTL_FONT_INACTIVE = "FONT_INACTIVE"
/*****************/
/* Gauge         */
/*****************/
public constant ICTL_SHOW_TEXT = "SHOW_TEXT"
public constant ICTL_DASHED = "DASHED"
public constant ICTL_MARGIN = "MARGIN"
public constant ICTL_TEXT = "TEXT"
/*****************/
/* Dial          */
/*****************/
public constant ICTL_DENSITY = "DENSITY"
--public constant ICTL_HORIZONTAL = "HORIZONTAL"
--public constant ICTL_VERTICAL = "VERTICAL"
public constant ICTL_CIRCULAR = "CIRCULAR"
public constant ICTL_UNIT = "UNIT"
/*****************/
/* Matrix        */
/*****************/
public constant IUP_ENTERITEM_CB = "ENTERITEM_CB"
public constant IUP_LEAVEITEM_CB = "LEAVEITEM_CB"
public constant IUP_EDITION_CB = "EDITION_CB"
public constant IUP_CLICK_CB = "CLICK_CB"
public constant IUP_DROP_CB = "DROP_CB"
public constant IUP_DROPSELECT_CB = "DROPSELECT_CB"
public constant IUP_DROPCHECK_CB = "DROPCHECK_CB"
--public constant IUP_SCROLL_CB = "SCROLL_CB"
public constant IUP_VALUE_CB = "VALUE_CB"
public constant IUP_VALUE_EDIT_CB = "VALUE_EDIT_CB"
public constant IUP_FIELD_CB = "FIELD_CB"
public constant IUP_RESIZEMATRIX = "RESIZEMATRIX"
public constant IUP_ADDLIN = "ADDLIN"
public constant IUP_ADDCOL = "ADDCOL"
public constant IUP_DELLIN = "DELLIN"
public constant IUP_DELCOL = "DELCOL"
public constant IUP_NUMLIN = "NUMLIN"
public constant IUP_NUMCOL = "NUMCOL"
public constant IUP_NUMLIN_VISIBLE = "NUMLIN_VISIBLE"
public constant IUP_NUMCOL_VISIBLE = "NUMCOL_VISIBLE"
public constant IUP_MARKED = "MARKED"
public constant IUP_WIDTHDEF = "WIDTHDEF"
public constant IUP_HEIGHTDEF = "HEIGHTDEF"
public constant IUP_AREA = "AREA"
public constant IUP_MARK_MODE = "MARK_MODE"
public constant IUP_LIN = "LIN"
public constant IUP_COL = "COL"
public constant IUP_LINCOL = "LINCOL"
--public constant IUP_CELL = "CELL"
public constant IUP_EDIT_MODE = "EDIT_MODE"
public constant IUP_FOCUS_CELL = "FOCUS_CELL"
--public constant IUP_ORIGIN = "ORIGIN"
public constant IUP_REDRAW = "REDRAW"
public constant IUP_PREVIOUSVALUE = "PREVIOUSVALUE"
public constant IUP_MOUSEMOVE_CB = "MOUSEMOVE_CB"
/*****************/
/* Tree          */
/*****************/
public constant IUP_ADDLEAF = "ADDLEAF"
public constant IUP_ADDBRANCH = "ADDBRANCH"
public constant IUP_DELNODE = "DELNODE"
public constant IUP_IMAGELEAF = "IMAGELEAF"
public constant IUP_IMAGEBRANCHCOLLAPSED = "IMAGEBRANCHCOLLAPSED"
public constant IUP_IMAGEBRANCHEXPANDED = "IMAGEBRANCHEXPANDED"
public constant IUP_IMAGEEXPANDED = "IMAGEEXPANDED"
public constant IUP_KIND = "KIND"
public constant IUP_PARENT = "PARENT"
public constant IUP_DEPTH = "DEPTH"
--public constant IUP_MARKED = "MARKED"
public constant IUP_ADDEXPANDED = "ADDEXPANDED"
public constant IUP_CTRL = "CTRL"
public constant IUP_SHIFT = "SHIFT"
public constant IUP_STATE = "STATE"
public constant IUP_STARTING = "STARTING"
public constant IUP_LEAF = "LEAF"
public constant IUP_BRANCH = "BRANCH"
public constant IUP_SELECTED = "SELECTED"
public constant IUP_CHILDREN = "CHILDREN"
--public constant IUP_MARKED = "MARKED"
public constant IUP_ROOT = "ROOT"
public constant IUP_LAST = "LAST"
public constant IUP_PGUP = "PGUP"
public constant IUP_PGDN = "PGDN"
public constant IUP_NEXT = "NEXT"
public constant IUP_PREVIOUS = "PREVIOUS"
public constant IUP_INVERT = "INVERT"
public constant IUP_BLOCK = "BLOCK"
public constant IUP_CLEARALL = "CLEARALL"
public constant IUP_MARKALL = "MARKALL"
public constant IUP_INVERTALL = "INVERTALL"
--public constant IUP_REDRAW = "REDRAW"
public constant IUP_COLLAPSED = "COLLAPSED"
public constant IUP_EXPANDED = "EXPANDED"
public constant IUP_SELECTION_CB = "SELECTION_CB"
public constant IUP_BRANCHOPEN_CB = "BRANCHOPEN_CB"
public constant IUP_BRANCHCLOSE_CB = "BRANCHCLOSE_CB"
public constant IUP_RIGHTCLICK_CB = "RIGHTCLICK_CB"
public constant IUP_EXECUTELEAF_CB = "EXECUTELEAF_CB"
public constant IUP_RENAMENODE_CB = "RENAMENODE_CB"
public constant IUP_IMGLEAF = "IMGLEAF"
public constant IUP_IMGCOLLAPSED = "IMGCOLLAPSED"
public constant IUP_IMGEXPANDED = "IMGEXPANDED"
public constant IUP_IMGBLANK = "IMGBLANK"
public constant IUP_IMGPAPER = "IMGPAPER"

public constant -- function delcarations
        xIupOpen                          = define_c_func(iup, "+IupOpen", {C_PTR,C_PTR}, C_INT),
        xIupClose                         = define_c_proc(iup, "+IupClose", {}),
        xIupImageLibOpen                  = define_c_proc(iupimglib, "+IupImageLibOpen", {}),
        xIupMainLoop                      = define_c_func(iup, "+IupMainLoop", {}, C_INT),
        xIupLoopStep                      = define_c_func(iup, "+IupLoopStep", {}, C_INT),
        xIupLoopStepWait                  = define_c_func(iup, "+IupLoopStepWait", {}, C_INT),
        xIupMainLoopLevel                 = define_c_func(iup, "+IupMainLoopLevel", {}, C_INT),
        xIupFlush                         = define_c_proc(iup, "+IupFlush", {}),
        xIupExitLoop                      = define_c_proc(iup, "+IupExitLoop", {}),
        xIupRecordInput                   = define_c_func(iup, "+IupRecordInput", {C_PTR,C_INT}, C_INT),
        xIupPlayInput                     = define_c_func(iup, "+IupPlayInput", {C_PTR}, C_INT),
        xIupUpdate                        = define_c_proc(iup, "+IupUpdate", {C_PTR}),
        xIupUpdateChildren                = define_c_proc(iup, "+IupUpdateChildren", {C_PTR}),
        xIupRedraw                        = define_c_proc(iup, "+IupRedraw", {C_PTR,C_INT}),
        xIupRefresh                       = define_c_proc(iup, "+IupRefresh", {C_PTR}),
        xIupRefreshChildren               = define_c_proc(iup, "+IupRefreshChildren", {C_PTR}),
        xIupHelp                          = define_c_func(iup, "+IupHelp", {C_PTR}, C_INT),
        xIupLoad                          = define_c_func(iup, "+IupLoad", {C_PTR}, C_PTR),
        xIupLoadBuffer                    = define_c_func(iup, "+IupLoadBuffer", {C_PTR}, C_PTR),
        xIupVersion                       = define_c_func(iup, "+IupVersion", {}, C_PTR),
        xIupVersionDate                   = define_c_func(iup, "+IupVersionDate", {}, C_PTR),
        xIupVersionNumber                 = define_c_func(iup, "+IupVersionNumber", {}, C_INT),
        xIupSetLanguage                   = define_c_proc(iup, "+IupSetLanguage", {C_PTR}),
        xIupGetLanguage                   = define_c_func(iup, "+IupGetLanguage", {}, C_PTR),
        xIupSetLanguageString             = define_c_proc(iup, "+IupSetLanguageString", {C_PTR,C_PTR}),
        xIupStoreLanguageString           = define_c_proc(iup, "+IupStoreLanguageString", {C_PTR,C_PTR}),
        xIupGetLanguageString             = define_c_func(iup, "+IupGetLanguageString", {C_PTR}, C_PTR),
        xIupSetLanguagePack               = define_c_proc(iup, "+IupSetLanguagePack", {C_PTR}),
        xIupDestroy                       = define_c_proc(iup, "+IupDestroy", {C_PTR}),
        xIupDetach                        = define_c_proc(iup, "+IupDetach", {C_PTR}),
        xIupAppend                        = define_c_func(iup, "+IupAppend", {C_PTR,C_PTR}, C_PTR),
        xIupInsert                        = define_c_func(iup, "+IupInsert", {C_PTR,C_PTR,C_PTR}, C_PTR),
        xIupGetChild                      = define_c_func(iup, "+IupGetChild", {C_PTR,C_INT}, C_PTR),
        xIupGetChildPos                   = define_c_func(iup, "+IupGetChildPos", {C_PTR,C_PTR}, C_INT),
        xIupGetChildCount                 = define_c_func(iup, "+IupGetChildCount", {C_PTR}, C_INT),
        xIupGetNextChild                  = define_c_func(iup, "+IupGetNextChild", {C_PTR,C_PTR}, C_PTR),
        xIupGetBrother                    = define_c_func(iup, "+IupGetBrother", {C_PTR}, C_PTR),
        xIupGetParent                     = define_c_func(iup, "+IupGetParent", {C_PTR}, C_PTR),
        xIupGetDialog                     = define_c_func(iup, "+IupGetDialog", {C_PTR}, C_PTR),
        xIupGetDialogChild                = define_c_func(iup, "+IupGetDialogChild", {C_PTR,C_PTR}, C_PTR),
        xIupReparent                      = define_c_func(iup, "+IupReparent", {C_PTR,C_PTR,C_PTR}, C_INT),
        xIupPopup                         = define_c_func(iup, "+IupPopup", {C_PTR,C_INT,C_INT}, C_INT),
        xIupShow                          = define_c_func(iup, "+IupShow", {C_PTR}, C_INT),
        xIupShowXY                        = define_c_func(iup, "+IupShowXY", {C_PTR,C_INT,C_INT}, C_INT),
        xIupHide                          = define_c_func(iup, "+IupHide", {C_PTR}, C_INT),
        xIupMap                           = define_c_func(iup, "+IupMap", {C_PTR}, C_INT),
        xIupUnmap                         = define_c_proc(iup, "+IupUnmap", {C_PTR}),
        xIupResetAttribute                = define_c_proc(iup, "+IupResetAttribute", {C_PTR,C_PTR}),
        xIupGetAllAttributes              = define_c_func(iup, "+IupGetAllAttributes", {C_PTR,C_PTR,C_INT}, C_INT),
        xIupSetAttributes                 = define_c_func(iup, "+IupSetAttributes", {C_PTR,C_PTR}, C_PTR),
        xIupGetAttributes                 = define_c_func(iup, "+IupGetAttributes", {C_PTR}, C_PTR),
        xIupSetAttribute                  = define_c_proc(iup, "+IupSetAttribute", {C_PTR,C_PTR,C_PTR}),
        xIupSetStrAttribute               = define_c_proc(iup, "+IupSetStrAttribute", {C_PTR,C_PTR,C_PTR}),
        xIupSetInt                        = define_c_proc(iup, "+IupSetInt", {C_PTR,C_PTR,C_INT}),
        xIupSetFloat                      = define_c_proc(iup, "+IupSetFloat", {C_PTR,C_PTR,C_FLOAT}),
        xIupSetDouble                     = define_c_proc(iup, "+IupSetDouble", {C_PTR,C_PTR,C_DOUBLE}),
        xIupSetRGB                        = define_c_proc(iup, "+IupSetRGB", {C_PTR,C_PTR,C_UCHAR,C_UCHAR,C_UCHAR}),
        xIupGetAttribute                  = define_c_func(iup, "+IupGetAttribute", {C_PTR,C_PTR}, C_PTR),
        xIupGetInt                        = define_c_func(iup, "+IupGetInt", {C_PTR,C_PTR}, C_INT),
        xIupGetInt2                       = define_c_func(iup, "+IupGetInt2", {C_PTR,C_PTR}, C_INT),
        xIupGetIntInt                     = define_c_func(iup, "+IupGetIntInt", {C_PTR,C_PTR,C_PTR,C_PTR}, C_INT),
        xIupGetFloat                      = define_c_func(iup, "+IupGetFloat", {C_PTR,C_PTR}, C_FLOAT),
        xIupGetDouble                     = define_c_func(iup, "+IupGetDouble", {C_PTR,C_PTR}, C_DOUBLE),
        xIupGetRGB                        = define_c_proc(iup, "+IupGetRGB", {C_PTR,C_PTR,C_PTR,C_PTR,C_PTR}),
        xIupSetAttributeId                = define_c_proc(iup, "+IupSetAttributeId", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupSetStrAttributeId             = define_c_proc(iup, "+IupSetStrAttributeId", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupSetIntId                      = define_c_proc(iup, "+IupSetIntId", {C_PTR,C_PTR,C_INT,C_INT}),
        xIupSetFloatId                    = define_c_proc(iup, "+IupSetFloatId", {C_PTR,C_PTR,C_INT,C_FLOAT}),
        xIupSetDoubleId                   = define_c_proc(iup, "+IupSetDoubleId", {C_PTR,C_PTR,C_INT,C_DOUBLE}),
        xIupSetRGBId                      = define_c_proc(iup, "+IupSetRGBId", {C_PTR,C_PTR,C_INT,C_UCHAR,C_UCHAR,C_UCHAR}),
        xIupGetAttributeId                = define_c_func(iup, "+IupGetAttributeId", {C_PTR,C_PTR,C_INT}, C_PTR),
        xIupGetIntId                      = define_c_func(iup, "+IupGetIntId", {C_PTR,C_PTR,C_INT}, C_INT),
        xIupGetFloatId                    = define_c_func(iup, "+IupGetFloatId", {C_PTR,C_PTR,C_INT}, C_FLOAT),
        xIupGetDoubleId                   = define_c_func(iup, "+IupGetDoubleId", {C_PTR,C_PTR,C_INT}, C_DOUBLE),
        xIupGetRGBId                      = define_c_proc(iup, "+IupGetRGBId", {C_PTR,C_PTR,C_INT,C_PTR,C_PTR,C_PTR}),
        xIupSetAttributeId2               = define_c_proc(iup, "+IupSetAttributeId2", {C_PTR,C_PTR,C_INT,C_INT,C_PTR}),
        xIupSetStrAttributeId2            = define_c_proc(iup, "+IupSetStrAttributeId2", {C_PTR,C_PTR,C_INT,C_INT,C_PTR}),
        xIupSetIntId2                     = define_c_proc(iup, "+IupSetIntId2", {C_PTR,C_PTR,C_INT,C_INT,C_INT}),
        xIupSetFloatId2                   = define_c_proc(iup, "+IupSetFloatId2", {C_PTR,C_PTR,C_INT,C_INT,C_FLOAT}),
        xIupSetDoubleId2                  = define_c_proc(iup, "+IupSetDoubleId2", {C_PTR,C_PTR,C_INT,C_INT,C_DOUBLE}),
        xIupSetRGBId2                     = define_c_proc(iup, "+IupSetRGBId2", {C_PTR,C_PTR,C_INT,C_INT,C_UCHAR,C_UCHAR,C_UCHAR}),
        xIupGetAttributeId2               = define_c_func(iup, "+IupGetAttributeId2", {C_PTR,C_PTR,C_INT,C_INT}, C_PTR),
        xIupGetIntId2                     = define_c_func(iup, "+IupGetIntId2", {C_PTR,C_PTR,C_INT,C_INT}, C_INT),
        xIupGetFloatId2                   = define_c_func(iup, "+IupGetFloatId2", {C_PTR,C_PTR,C_INT,C_INT}, C_FLOAT),
        xIupGetDoubleId2                  = define_c_func(iup, "+IupGetDoubleId2", {C_PTR,C_PTR,C_INT,C_INT}, C_DOUBLE),
        xIupGetRGBId2                     = define_c_proc(iup, "+IupGetRGBId2", {C_PTR,C_PTR,C_INT,C_INT,C_PTR,C_PTR,C_PTR}),
        xIupSetGlobal                     = define_c_proc(iup, "+IupSetGlobal", {C_PTR,C_PTR}),
        xIupSetStrGlobal                  = define_c_proc(iup, "+IupSetStrGlobal", {C_PTR,C_PTR}),
        xIupGetGlobal                     = define_c_func(iup, "+IupGetGlobal", {C_PTR}, C_PTR),
        xIupSetFocus                      = define_c_func(iup, "+IupSetFocus", {C_PTR}, C_PTR),
        xIupGetFocus                      = define_c_func(iup, "+IupGetFocus", {}, C_PTR),
        xIupPreviousField                 = define_c_func(iup, "+IupPreviousField", {C_PTR}, C_PTR),
        xIupNextField                     = define_c_func(iup, "+IupNextField", {C_PTR}, C_PTR),
        xIupGetCallback                   = define_c_func(iup, "+IupGetCallback", {C_PTR,C_PTR}, C_PTR),
        xIupSetCallback                   = define_c_func(iup, "+IupSetCallback", {C_PTR,C_PTR,C_PTR}, C_PTR),
        xIupGetFunction                   = define_c_func(iup, "+IupGetFunction", {C_PTR}, C_PTR),
        xIupSetFunction                   = define_c_func(iup, "+IupSetFunction", {C_PTR,C_PTR}, C_PTR),
        xIupGetHandle                     = define_c_func(iup, "+IupGetHandle", {C_PTR}, C_PTR),
        xIupSetHandle                     = define_c_func(iup, "+IupSetHandle", {C_PTR,C_PTR}, C_PTR),
        xIupGetAllNames                   = define_c_func(iup, "+IupGetAllNames", {C_PTR,C_INT}, C_INT),
        xIupGetAllDialogs                 = define_c_func(iup, "+IupGetAllDialogs", {C_PTR,C_INT}, C_INT),
        xIupGetName                       = define_c_func(iup, "+IupGetName", {C_PTR}, C_PTR),
        xIupSetAttributeHandle            = define_c_proc(iup, "+IupSetAttributeHandle", {C_PTR,C_PTR,C_PTR}),
        xIupGetAttributeHandle            = define_c_func(iup, "+IupGetAttributeHandle", {C_PTR,C_PTR}, C_PTR),
        xIupGetClassName                  = define_c_func(iup, "+IupGetClassName", {C_PTR}, C_PTR),
        xIupGetClassType                  = define_c_func(iup, "+IupGetClassType", {C_PTR}, C_PTR),
        xIupGetAllClasses                 = define_c_func(iup, "+IupGetAllClasses", {C_PTR,C_INT}, C_INT),
        xIupGetClassAttributes            = define_c_func(iup, "+IupGetClassAttributes", {C_PTR,C_PTR,C_INT}, C_INT),
        xIupGetClassCallbacks             = define_c_func(iup, "+IupGetClassCallbacks", {C_PTR,C_PTR,C_INT}, C_INT),
        xIupSaveClassAttributes           = define_c_proc(iup, "+IupSaveClassAttributes", {C_PTR}),
        xIupCopyClassAttributes           = define_c_proc(iup, "+IupCopyClassAttributes", {C_PTR,C_PTR}),
        xIupSetClassDefaultAttribute      = define_c_proc(iup, "+IupSetClassDefaultAttribute", {C_PTR,C_PTR,C_PTR}),
        xIupClassMatch                    = define_c_func(iup, "+IupClassMatch", {C_PTR,C_PTR}, C_INT),
        xIupCreate                        = define_c_func(iup, "+IupCreatev", {C_PTR,C_PTR}, C_PTR),
        xIupFill                          = define_c_func(iup, "+IupFill", {}, C_PTR),
        xIupRadio                         = define_c_func(iup, "+IupRadio", {C_PTR}, C_PTR),
        xIupVbox                          = define_c_func(iup, "+IupVboxv", {C_PTR}, C_PTR),
        xIupZbox                          = define_c_func(iup, "+IupZboxv", {C_PTR}, C_PTR),
        xIupHbox                          = define_c_func(iup, "+IupHboxv", {C_PTR}, C_PTR),
        xIupNormalizer                    = define_c_func(iup, "+IupNormalizerv", {C_PTR}, C_PTR),
        xIupCbox                          = define_c_func(iup, "+IupCboxv", {C_PTR}, C_PTR),
        xIupSbox                          = define_c_func(iup, "+IupSbox", {C_PTR}, C_PTR),
        xIupSplit                         = define_c_func(iup, "+IupSplit", {C_PTR,C_PTR}, C_PTR),
        xIupScrollBox                     = define_c_func(iup, "+IupScrollBox", {C_PTR}, C_PTR),
        xIupGridBox                       = define_c_func(iup, "+IupGridBoxv", {C_PTR}, C_PTR),
        xIupExpander                      = define_c_func(iup, "+IupExpander", {C_PTR}, C_PTR),
        xIupDetachBox                     = define_c_func(iup, "+IupDetachBox", {C_PTR}, C_PTR),
        xIupBackgroundBox                 = define_c_func(iup, "+IupBackgroundBox", {C_PTR}, C_PTR),
        xIupFrame                         = define_c_func(iup, "+IupFrame", {C_PTR}, C_PTR),
        xIupImage                         = define_c_func(iup, "+IupImage", {C_INT,C_INT,C_PTR}, C_PTR),
        xIupImageRGB                      = define_c_func(iup, "+IupImageRGB", {C_INT,C_INT,C_PTR}, C_PTR),
        xIupImageRGBA                     = define_c_func(iup, "+IupImageRGBA", {C_INT,C_INT,C_PTR}, C_PTR),
        xIupItem                          = define_c_func(iup, "+IupItem", {C_PTR,C_PTR}, C_PTR),
        xIupSubmenu                       = define_c_func(iup, "+IupSubmenu", {C_PTR,C_PTR}, C_PTR),
        xIupSeparator                     = define_c_func(iup, "+IupSeparator", {}, C_PTR),
        xIupMenu                          = define_c_func(iup, "+IupMenuv", {C_PTR}, C_PTR),
        xIupButton                        = define_c_func(iup, "+IupButton", {C_PTR,C_PTR}, C_PTR),
        xIupCanvas                        = define_c_func(iup, "+IupCanvas", {C_PTR}, C_PTR),
        xIupDialog                        = define_c_func(iup, "+IupDialog", {C_PTR}, C_PTR),
        xIupUser                          = define_c_func(iup, "+IupUser", {}, C_PTR),
        xIupLabel                         = define_c_func(iup, "+IupLabel", {C_PTR}, C_PTR),
        xIupList                          = define_c_func(iup, "+IupList", {C_PTR}, C_PTR),
        xIupText                          = define_c_func(iup, "+IupText", {C_PTR}, C_PTR),
        xIupMultiLine                     = define_c_func(iup, "+IupMultiLine", {C_PTR}, C_PTR),
        xIupToggle                        = define_c_func(iup, "+IupToggle", {C_PTR,C_PTR}, C_PTR),
        xIupTimer                         = define_c_func(iup, "+IupTimer", {}, C_PTR),
        xIupClipboard                     = define_c_func(iup, "+IupClipboard", {}, C_PTR),
        xIupProgressBar                   = define_c_func(iup, "+IupProgressBar", {}, C_PTR),
        xIupVal                           = define_c_func(iup, "+IupVal", {C_PTR}, C_PTR),
        xIupTabs                          = define_c_func(iup, "+IupTabsv", {C_PTR}, C_PTR),
        xIupTree                          = define_c_func(iup, "+IupTree", {}, C_PTR),
        xIupLink                          = define_c_func(iup, "+IupLink", {C_PTR,C_PTR}, C_PTR),
        xIupFlatButton                    = define_c_func(iup, "+IupFlatButton", {C_PTR}, C_PTR),
        xIupSpin                          = define_c_func(iup, "+IupSpin", {}, C_PTR),
        xIupSpinbox                       = define_c_func(iup, "+IupSpinbox", {C_PTR}, C_PTR),
        xIupSaveImageAsText               = define_c_func(iup, "+IupSaveImageAsText", {C_PTR,C_PTR,C_PTR,C_PTR}, C_INT),
        xIupTextConvertLinColToPos        = define_c_proc(iup, "+IupTextConvertLinColToPos", {C_PTR,C_INT,C_INT,C_PTR}),
        xIupTextConvertPosToLinCol        = define_c_proc(iup, "+IupTextConvertPosToLinCol", {C_PTR,C_INT,C_PTR,C_PTR}),
        xIupConvertXYToPos                = define_c_func(iup, "+IupConvertXYToPos", {C_PTR,C_INT,C_INT}, C_INT),
        xIupStoreGlobal                   = define_c_proc(iup, "+IupStoreGlobal", {C_PTR,C_PTR}),
        xIupStoreAttribute                = define_c_proc(iup, "+IupStoreAttribute", {C_PTR,C_PTR,C_PTR}),
        xIupStoreAttributeId              = define_c_proc(iup, "+IupStoreAttributeId", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupStoreAttributeId2             = define_c_proc(iup, "+IupStoreAttributeId2", {C_PTR,C_PTR,C_INT,C_INT,C_PTR}),
        xIupTreeSetUserId                 = define_c_func(iup, "+IupTreeSetUserId", {C_PTR,C_INT,C_PTR}, C_INT),
        xIupTreeGetUserId                 = define_c_func(iup, "+IupTreeGetUserId", {C_PTR,C_INT}, C_PTR),
        xIupTreeGetId                     = define_c_func(iup, "+IupTreeGetId", {C_PTR,C_PTR}, C_INT),
        xIupTreeSetAttributeHandle        = define_c_proc(iup, "+IupTreeSetAttributeHandle", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupTreeSetAttribute              = define_c_proc(iup, "+IupTreeSetAttribute", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupTreeStoreAttribute            = define_c_proc(iup, "+IupTreeStoreAttribute", {C_PTR,C_PTR,C_INT,C_PTR}),
        xIupTreeGetAttribute              = define_c_func(iup, "+IupTreeGetAttribute", {C_PTR,C_PTR,C_INT}, C_PTR),
        xIupTreeGetInt                    = define_c_func(iup, "+IupTreeGetInt", {C_PTR,C_PTR,C_INT}, C_INT),
        xIupTreeGetFloat                  = define_c_func(iup, "+IupTreeGetFloat", {C_PTR,C_PTR,C_INT}, C_FLOAT),
        xIupGetActionName                 = define_c_func(iup, "+IupGetActionName", {}, C_PTR),
        xIupMapFont                       = define_c_func(iup, "+IupMapFont", {C_PTR}, C_PTR),
        xIupUnMapFont                     = define_c_func(iup, "+IupUnMapFont", {C_PTR}, C_PTR),
        xIupFileDlg                       = define_c_func(iup, "+IupFileDlg", {}, C_PTR),
        xIupMessageDlg                    = define_c_func(iup, "+IupMessageDlg", {}, C_PTR),
        xIupColorDlg                      = define_c_func(iup, "+IupColorDlg", {}, C_PTR),
        xIupFontDlg                       = define_c_func(iup, "+IupFontDlg", {}, C_PTR),
        xIupProgressDlg                   = define_c_func(iup, "+IupProgressDlg", {}, C_PTR),
        xIupGetFile                       = define_c_func(iup, "+IupGetFile", {C_PTR}, C_INT),
        xIupMessage                       = define_c_proc(iup, "+IupMessage", {C_PTR,C_PTR}),
        xIupAlarm                         = define_c_func(iup, "+IupAlarm", {C_PTR,C_PTR,C_PTR,C_PTR,C_PTR}, C_INT),
        xIupListDialog                    = define_c_func(iup, "+IupListDialog", {C_INT,C_PTR,C_INT,C_PTR,C_INT,C_INT,C_INT,C_PTR}, C_INT),
        xIupGetText                       = define_c_func(iup, "+IupGetText", {C_PTR,C_PTR}, C_INT),
        xIupGetColor                      = define_c_func(iup, "+IupGetColor", {C_INT,C_INT,C_PTR,C_PTR,C_PTR}, C_INT),
        xIupGetParam                      = define_c_func(iup, "+IupGetParamv", {C_PTR,C_PTR,C_PTR,C_PTR,C_INT,C_INT,C_PTR}, C_INT),
        xIupParamf                        = define_c_func(iup, "+IupParamf", {C_PTR}, C_PTR),
        xIupParamBox                      = define_c_func(iup, "+IupParamBox", {C_PTR,C_PTR,C_INT}, C_PTR),
        xIupLayoutDialog                  = define_c_func(iup, "+IupLayoutDialog", {C_PTR}, C_PTR),
        xIupElementPropertiesDialog       = define_c_func(iup, "+IupElementPropertiesDialog", {C_PTR}, C_PTR),
$

if xIupSetCallback=0 then ?9/0 end if

public constant IUP_NAME = "IUP - Portable User Interface"
public constant IUP_DESCRIPTION = "Multi-platform Toolkit for Building Graphical User Interfaces"
public constant IUP_COPYRIGHT = "Copyright (C) 1994-2015 Tecgraf/PUC-Rio"
public constant IUP_VERSION = "3.16" /* bug fixes are reported only by IupVersion functions */
public constant IUP_VERSION_NUMBER = 316000
public constant IUP_VERSION_DATE = "2015/09/15" /* does not include bug fix releases */

/************************************************************************/
/*                   Common Flags and Return Values                     */
/************************************************************************/
public constant IUP_ERROR = 1
public constant IUP_NOERROR = 0
public constant IUP_OPENED = -1
public constant IUP_INVALID = -1
public constant IUP_INVALID_ID = -10

/************************************************************************/
/*                        Main API                                      */
/************************************************************************/
--int IupOpen(int *argc, char ***argv);
global procedure IupOpen()
    if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then
        ?9/0
    end if
end procedure

--public function IupOpen(atom argc, sequence argv = {})
--atom p_argv = allocate_string_pointer_array(argv)
--atom result = c_func(xIupOpen, {argc,p_argv})
--  free(p_argv)
--  return result
--end function
--
--void IupClose(void);
public procedure IupClose()
    c_proc(xIupClose, {})
end procedure

--void IupImageLibOpen(void);
public procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure

--int IupMainLoop(void);
public function IupMainLoop()
atom result = c_func(xIupMainLoop, {})
    return result
end function

--int IupLoopStep(void);
public function IupLoopStep()
atom result = c_func(xIupLoopStep, {})
    return result
end function

--int IupLoopStepWait(void);
public function IupLoopStepWait()
atom result = c_func(xIupLoopStepWait, {})
    return result
end function

--int IupMainLoopLevel(void);
public function IupMainLoopLevel()
atom result = c_func(xIupMainLoopLevel, {})
    return result
end function

--void IupFlush(void);
public procedure IupFlush()
    c_proc(xIupFlush, {})
end procedure

--void IupExitLoop(void);
public procedure IupExitLoop()
    c_proc(xIupExitLoop, {})
end procedure

--int IupRecordInput(const char* filename, int mode);
public function IupRecordInput(object filename, atom mode)
    if sequence(filename) then filename = allocate_string(filename) end if
    atom result = c_func(xIupRecordInput, {filename,mode})
    return result
end function

--int IupPlayInput(const char* filename);
public function IupPlayInput(object filename = NULL)
    if sequence(filename) then filename = allocate_string(filename) end if
    atom result = c_func(xIupPlayInput, {filename})
    return result
end function

--void IupUpdate(Ihandle* ih);
public procedure IupUpdate(atom ih)
    c_proc(xIupUpdate, {ih})
end procedure

--void IupUpdateChildren(Ihandle* ih);
public procedure IupUpdateChildren(atom ih)
    c_proc(xIupUpdateChildren, {ih})
end procedure

--void IupRedraw(Ihandle* ih, int children);
public procedure IupRedraw(atom ih, atom children)
    c_proc(xIupRedraw, {ih,children})
end procedure

--void IupRefresh(Ihandle* ih);
public procedure IupRefresh(atom ih)
    c_proc(xIupRefresh, {ih})
end procedure

--void IupRefreshChildren(Ihandle* ih);
public procedure IupRefreshChildren(atom ih)
    c_proc(xIupRefreshChildren, {ih})
end procedure

--int IupHelp(const char* url);
public function IupHelp(object url = NULL)
    if sequence(url) then url = allocate_string(url) end if
    atom result = c_func(xIupHelp, {url})
    return result
end function

--char* IupLoad(const char *filename);
public function IupLoad(object filename = NULL)
    if sequence(filename) then filename = allocate_string(filename) end if
    atom ptr = c_func(xIupLoad, {filename})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupLoadBuffer(const char *buffer);
public function IupLoadBuffer(object buffer = NULL)
    if sequence(buffer) then buffer = allocate_string(buffer) end if
    atom ptr = c_func(xIupLoadBuffer, {buffer})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupVersion(void);
public function IupVersion()
atom ptr = c_func(xIupVersion, {})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupVersionDate(void);
public function IupVersionDate()
atom ptr = c_func(xIupVersionDate, {})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupVersionNumber(void);
public function IupVersionNumber()
atom result = c_func(xIupVersionNumber, {})
    return result
end function

--void IupSetLanguage(const char *lng);
public procedure IupSetLanguage(object lng = NULL)
    if sequence(lng) then lng = allocate_string(lng) end if
    c_proc(xIupSetLanguage, {lng})
end procedure

--char* IupGetLanguage(void);
public function IupGetLanguage()
atom ptr = c_func(xIupGetLanguage, {})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--void IupSetLanguageString(const char* name, const char* str);
public procedure IupSetLanguageString(object name = NULL, object str = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(str) then str = allocate_string(str) end if
    c_proc(xIupSetLanguageString, {name,str})
end procedure

--void IupStoreLanguageString(const char* name, const char* str);
public procedure IupStoreLanguageString(object name = NULL, object str = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(str) then str = allocate_string(str) end if
    c_proc(xIupStoreLanguageString, {name,str})
end procedure

--char* IupGetLanguageString(const char* name);
public function IupGetLanguageString(object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupGetLanguageString, {name})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--void IupSetLanguagePack(Ihandle* ih);
public procedure IupSetLanguagePack(atom ih)
    c_proc(xIupSetLanguagePack, {ih})
end procedure

--void IupDestroy(Ihandle* ih);
public procedure IupDestroy(atom ih)
    c_proc(xIupDestroy, {ih})
end procedure

--void IupDetach(Ihandle* child);
public procedure IupDetach(atom child)
    c_proc(xIupDetach, {child})
end procedure

--Ihandle* IupAppend(Ihandle* ih, Ihandle* child);
public function IupAppend(atom ih, atom child)
atom result = c_func(xIupAppend, {ih,child})
    return result
end function

--Ihandle* IupInsert(Ihandle* ih, Ihandle* ref_child, Ihandle* child);
public function IupInsert(atom ih, atom ref_child, atom child)
atom result = c_func(xIupInsert, {ih,ref_child,child})
    return result
end function

--Ihandle* IupGetChild(Ihandle* ih, int pos);
public function IupGetChild(atom ih, atom pos)
atom result = c_func(xIupGetChild, {ih,pos})
    return result
end function

--int IupGetChildPos(Ihandle* ih, Ihandle* child);
public function IupGetChildPos(atom ih, atom child)
atom result = c_func(xIupGetChildPos, {ih,child})
    return result
end function

--int IupGetChildCount(Ihandle* ih);
public function IupGetChildCount(atom ih)
atom result = c_func(xIupGetChildCount, {ih})
    return result
end function

--Ihandle* IupGetNextChild(Ihandle* ih, Ihandle* child);
public function IupGetNextChild(atom ih, atom child)
atom result = c_func(xIupGetNextChild, {ih,child})
    return result
end function

--Ihandle* IupGetBrother(Ihandle* ih);
public function IupGetBrother(atom ih)
atom result = c_func(xIupGetBrother, {ih})
    return result
end function

--Ihandle* IupGetParent(Ihandle* ih);
public function IupGetParent(atom ih)
atom result = c_func(xIupGetParent, {ih})
    return result
end function

--Ihandle* IupGetDialog(Ihandle* ih);
public function IupGetDialog(atom ih)
atom result = c_func(xIupGetDialog, {ih})
    return result
end function

--Ihandle* IupGetDialogChild(Ihandle* ih, const char* name);
public function IupGetDialogChild(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetDialogChild, {ih,name})
    return result
end function

--int IupReparent(Ihandle* ih, Ihandle* new_parent, Ihandle* ref_child);
public function IupReparent(atom ih, atom new_parent, atom ref_child)
atom result = c_func(xIupReparent, {ih,new_parent,ref_child})
    return result
end function

--int IupPopup(Ihandle* ih, int x, int y);
public function IupPopup(atom ih, atom x, atom y)
atom result = c_func(xIupPopup, {ih,x,y})
    return result
end function

--int IupShow(Ihandle* ih);
public function IupShow(atom ih)
atom result = c_func(xIupShow, {ih})
    return result
end function

--int IupShowXY(Ihandle* ih, int x, int y);
--public function IupShowXY(atom ih, atom x, atom y)
--atom result = c_func(xIupShowXY, {ih,x,y})
--  return result
--end function
global procedure IupShowXY(Ihandle ih, integer x, integer y)
    if c_func(xIupShowXY, {ih, x, y})!=IUP_NOERROR then ?9/0 end if
end procedure

--int IupHide(Ihandle* ih);
public function IupHide(atom ih)
atom result = c_func(xIupHide, {ih})
    return result
end function

--int IupMap(Ihandle* ih);
public function IupMap(atom ih)
atom result = c_func(xIupMap, {ih})
    return result
end function

--void IupUnmap(Ihandle *ih);
public procedure IupUnmap(atom ih)
    c_proc(xIupUnmap, {ih})
end procedure

--void IupResetAttribute(Ihandle *ih, const char* name);
public procedure IupResetAttribute(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupResetAttribute, {ih,name})
end procedure

--int IupGetAllAttributes(Ihandle* ih, char** names, int n);
public function IupGetAllAttributes(atom ih)
atom n = c_func(xIupGetAllAttributes, {ih,NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetAllAttributes, {ih,ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--Ihandle* IupSetAttributes(Ihandle* ih, const char *str);
--public function IupSetAttributes(atom ih, object str = NULL)
--  if sequence(str) then str = allocate_string(str,1) end if
--  atom result = c_func(xIupSetAttributes, {ih,str})
--  return result
--end function
global procedure IupSetAttributes(Ihandle ih, string attributes, sequence data = {})
    if length(data) then
        attributes = sprintf(attributes, data)
    end if
    ih = c_func(xIupSetAttributes, {ih, attributes})
end procedure

global function IupSetAttributesf(Ihandle ih, string attributes, sequence data = {})
    IupSetAttributes(ih, attributes, data)
    return ih
end function

--char* IupGetAttributes(Ihandle* ih);
public function IupGetAttributes(atom ih)
atom ptr = c_func(xIupGetAttributes, {ih})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--void IupSetAttribute(Ihandle* ih, const char* name, const char* v);
public procedure IupSetAttribute(atom ih, object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetAttribute, {ih,name,v})
end procedure

--NO!
--global procedure IupSetAttributef(atom ih, sequence name, string fmt, sequence data)
--atom pName = allocate_string(name,1)
--atom pValue = allocate_string(sprintf(fmt,data),1)
--  c_proc(xIupSetAttribute, {ih, pName, pValue})
--end procedure

--void IupSetStrAttribute(Ihandle* ih, const char* name, const char* v);
public procedure IupSetStrAttribute(atom ih, object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetStrAttribute, {ih,name,v})
end procedure

--void IupSetInt(Ihandle* ih, const char* name, int v);
public procedure IupSetInt(atom ih, object name, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetInt, {ih,name,v})
end procedure

--void IupSetFloat(Ihandle* ih, const char* name, float v);
public procedure IupSetFloat(atom ih, object name, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetFloat, {ih,name,v})
end procedure

--void IupSetDouble(Ihandle* ih, const char* name, double v);
public procedure IupSetDouble(atom ih, object name, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetDouble, {ih,name,v})
end procedure

--void IupSetRGB(Ihandle *ih, const char* name, unsigned char r, unsigned char g, unsigned char b);
public procedure IupSetRGB(atom ih, object name, atom r, atom g, atom b)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetRGB, {ih,name,r,g,b})
end procedure

--char* IupGetAttribute(Ihandle* ih, const char* name);
public function IupGetAttribute(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupGetAttribute, {ih,name})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupGetInt(Ihandle* ih, const char* name);
public function IupGetInt(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetInt, {ih,name})
    return result
end function

--int IupGetInt2(Ihandle* ih, const char* name);
public function IupGetInt2(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetInt2, {ih,name})
    return result
end function

--int IupGetIntInt(Ihandle *ih, const char* name, int *i1, int *i2);
public function IupGetIntInt(atom ih, object name, atom i1, atom i2)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetIntInt, {ih,name,i1,i2})
    return result
end function

--float IupGetFloat(Ihandle* ih, const char* name);
public function IupGetFloat(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetFloat, {ih,name})
    return result
end function

--double IupGetDouble(Ihandle* ih, const char* name);
public function IupGetDouble(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetDouble, {ih,name})
    return result
end function

--void IupGetRGB(Ihandle *ih, const char* name, unsigned char *r, unsigned char *g, unsigned char *b);
public function IupGetRGB(atom ih, object name = NULL)
atom rgb = allocate_data(sizeof(C_INT)*3, 1)
atom r = rgb+sizeof(C_INT)*0
atom g = rgb+sizeof(C_INT)*1
atom b = rgb+sizeof(C_INT)*2
    c_proc(xIupGetRGB, {ih,name,r,g,b})
    return peek({rgb, 3})
end function

--void IupSetAttributeId(Ihandle *ih, const char* name, int id, const char *v);
public procedure IupSetAttributeId(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetAttributeId, {ih,name,id,v})
end procedure

--void IupSetStrAttributeId(Ihandle *ih, const char* name, int id, const char *v);
public procedure IupSetStrAttributeId(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetStrAttributeId, {ih,name,id,v})
end procedure

--void IupSetIntId(Ihandle* ih, const char* name, int id, int v);
public procedure IupSetIntId(atom ih, object name, atom id, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetIntId, {ih,name,id,v})
end procedure

--void IupSetFloatId(Ihandle* ih, const char* name, int id, float v);
public procedure IupSetFloatId(atom ih, object name, atom id, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetFloatId, {ih,name,id,v})
end procedure

--void IupSetDoubleId(Ihandle* ih, const char* name, int id, double v);
public procedure IupSetDoubleId(atom ih, object name, atom id, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetDoubleId, {ih,name,id,v})
end procedure

--void IupSetRGBId(Ihandle *ih, const char* name, int id, unsigned char r, unsigned char g, unsigned char b);
public procedure IupSetRGBId(atom ih, object name, atom id, atom r, atom g, atom b)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetRGBId, {ih,name,id,r,g,b})
end procedure

--char* IupGetAttributeId(Ihandle *ih, const char* name, int id);
public function IupGetAttributeId(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupGetAttributeId, {ih,name,id})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupGetIntId(Ihandle *ih, const char* name, int id);
public function IupGetIntId(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetIntId, {ih,name,id})
    return result
end function

--float IupGetFloatId(Ihandle *ih, const char* name, int id);
public function IupGetFloatId(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetFloatId, {ih,name,id})
    return result
end function

--double IupGetDoubleId(Ihandle *ih, const char* name, int id);
public function IupGetDoubleId(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetDoubleId, {ih,name,id})
    return result
end function

--void IupGetRGBId(Ihandle *ih, const char* name, int id, unsigned char *r, unsigned char *g, unsigned char *b);
public function IupGetRGBId(atom ih, object name, atom id)
atom rgb = allocate_data(sizeof(C_INT)*3, 1)
atom r = rgb+sizeof(C_INT)*0
atom g = rgb+sizeof(C_INT)*1
atom b = rgb+sizeof(C_INT)*2
    c_proc(xIupGetRGBId, {ih,name,id,r,g,b})
    return peek({rgb, 3})
end function

--void IupSetAttributeId2(Ihandle* ih, const char* name, int lin, int col, const char* v);
public procedure IupSetAttributeId2(atom ih, object name, atom lin, atom col, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetAttributeId2, {ih,name,lin,col,v})
end procedure

--void IupSetStrAttributeId2(Ihandle* ih, const char* name, int lin, int col, const char* v);
public procedure IupSetStrAttributeId2(atom ih, object name, atom lin, atom col, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetStrAttributeId2, {ih,name,lin,col,v})
end procedure

--void IupSetIntId2(Ihandle* ih, const char* name, int lin, int col, int v);
public procedure IupSetIntId2(atom ih, object name, atom lin, atom col, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetIntId2, {ih,name,lin,col,v})
end procedure

--void IupSetFloatId2(Ihandle* ih, const char* name, int lin, int col, float v);
public procedure IupSetFloatId2(atom ih, object name, atom lin, atom col, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetFloatId2, {ih,name,lin,col,v})
end procedure

--void IupSetDoubleId2(Ihandle* ih, const char* name, int lin, int col, double v);
public procedure IupSetDoubleId2(atom ih, object name, atom lin, atom col, atom v)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetDoubleId2, {ih,name,lin,col,v})
end procedure

--void IupSetRGBId2(Ihandle *ih, const char* name, int lin, int col, unsigned char r, unsigned char g, unsigned char b);
public procedure IupSetRGBId2(atom ih, object name, atom lin, atom col, atom r, atom g, atom b)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetRGBId2, {ih,name,lin,col,r,g,b})
end procedure

--char* IupGetAttributeId2(Ihandle* ih, const char* name, int lin, int col);
public function IupGetAttributeId2(atom ih, object name, atom lin, atom col)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupGetAttributeId2, {ih,name,lin,col})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupGetIntId2(Ihandle* ih, const char* name, int lin, int col);
public function IupGetIntId2(atom ih, object name, atom lin, atom col)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetIntId2, {ih,name,lin,col})
    return result
end function

--float IupGetFloatId2(Ihandle* ih, const char* name, int lin, int col);
public function IupGetFloatId2(atom ih, object name, atom lin, atom col)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetFloatId2, {ih,name,lin,col})
    return result
end function

--double IupGetDoubleId2(Ihandle* ih, const char* name, int lin, int col);
public function IupGetDoubleId2(atom ih, object name, atom lin, atom col)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetDoubleId2, {ih,name,lin,col})
    return result
end function

--void IupGetRGBId2(Ihandle *ih, const char* name, int lin, int col, unsigned char *r, unsigned char *g, unsigned char *b);
public function IupGetRGBId2(atom ih, object name, atom lin, atom col)
atom rgb = allocate_data(sizeof(C_INT)*3, 1)
atom r = rgb+sizeof(C_INT)*0
atom g = rgb+sizeof(C_INT)*1
atom b = rgb+sizeof(C_INT)*2
    c_proc(xIupGetRGBId2, {ih,name,lin,col,r,g,b})
    return peek({rgb, 3})
end function

--void IupSetGlobal(const char* name, const char* v);
public procedure IupSetGlobal(object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetGlobal, {name,v})
end procedure

--void IupSetStrGlobal(const char* name, const char* v);
public procedure IupSetStrGlobal(object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetStrGlobal, {name,v})
end procedure

--char* IupGetGlobal(const char* name);
public function IupGetGlobal(object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupGetGlobal, {name})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--Ihandle* IupSetFocus(Ihandle* ih);
public function IupSetFocus(atom ih)
atom result = c_func(xIupSetFocus, {ih})
    return result
end function

--Ihandle* IupGetFocus(void);
public function IupGetFocus()
atom result = c_func(xIupGetFocus, {})
    return result
end function

--Ihandle* IupPreviousField(Ihandle* ih);
public function IupPreviousField(atom ih)
atom result = c_func(xIupPreviousField, {ih})
    return result
end function

--Ihandle* IupNextField(Ihandle* ih);
public function IupNextField(atom ih)
atom result = c_func(xIupNextField, {ih})
    return result
end function

--Icallback IupGetCallback(Ihandle* ih, const char *name);
public function IupGetCallback(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetCallback, {ih,name})
    return result
end function

public procedure IupSetCallback(atom ih, object name, atom func)
    if sequence(name) then name = allocate_string(name,1) end if
--  atom result = c_func(xIupSetCallback, {ih,name,call_back(func)})
    atom result = c_func(xIupSetCallback, {ih,name,func})
end procedure

--Icallback IupSetCallback(Ihandle* ih, const char *name, Icallback func);
public function IupSetCallbackf(atom ih, object name, atom func)
    if sequence(name) then name = allocate_string(name,1) end if
--  atom result = c_func(xIupSetCallback, {ih,name,call_back(func)})
    atom result = c_func(xIupSetCallback, {ih,name,func})
    return result
end function

--Icallback IupGetFunction(const char *name);
public function IupGetFunction(object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetFunction, {name})
    return result
end function

--Icallback IupSetFunction(const char *name, Icallback func);
public function IupSetFunction(object name, atom func)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupSetFunction, {name,func})
    return result
end function

--Ihandle* IupGetHandle(const char *name);
public function IupGetHandle(object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetHandle, {name})
    return result
end function

--Ihandle* IupSetHandle(const char *name, Ihandle* ih);
public function IupSetHandle(object name, atom ih)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupSetHandle, {name,ih})
    return result
end function

--int IupGetAllNames(char** names, int n);
public function IupGetAllNames()
atom n = c_func(xIupGetAllNames, {NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetAllNames, {ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--int IupGetAllDialogs(char** names, int n);
public function IupGetAllDialogs()
atom n = c_func(xIupGetAllDialogs, {NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetAllDialogs, {ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--char* IupGetName(Ihandle* ih);
public function IupGetName(atom ih)
atom ptr = c_func(xIupGetName, {ih})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--void IupSetAttributeHandle(Ihandle* ih, const char* name, Ihandle* ih_named);
public procedure IupSetAttributeHandle(atom ih, object name, atom ih_named)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupSetAttributeHandle, {ih,name,ih_named})
end procedure

--Ihandle* IupGetAttributeHandle(Ihandle* ih, const char* name);
public function IupGetAttributeHandle(atom ih, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupGetAttributeHandle, {ih,name})
    return result
end function

--char* IupGetClassName(Ihandle* ih);
public function IupGetClassName(atom ih)
atom ptr = c_func(xIupGetClassName, {ih})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupGetClassType(Ihandle* ih);
public function IupGetClassType(atom ih)
atom ptr = c_func(xIupGetClassType, {ih})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupGetAllClasses(char** names, int n);
public function IupGetAllClasses()
atom n = c_func(xIupGetAllClasses, {NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetAllClasses, {ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--int IupGetClassAttributes(const char* classname, char** names, int n);
public function IupGetClassAttributes(object classname = NULL)
atom n = c_func(xIupGetClassAttributes, {classname,NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetClassAttributes, {classname,ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--int IupGetClassCallbacks(const char* classname, char** names, int n);
public function IupGetClassCallbacks(object classname = NULL)
atom n = c_func(xIupGetClassCallbacks, {classname,NULL,0})
atom ptr = allocate_data(sizeof(C_PTR)*n, 1)
    n = c_func(xIupGetClassCallbacks, {classname,ptr,n})
    return peek_string_pointer_array(ptr, n)
end function

--void IupSaveClassAttributes(Ihandle* ih);
public procedure IupSaveClassAttributes(atom ih)
    c_proc(xIupSaveClassAttributes, {ih})
end procedure

--void IupCopyClassAttributes(Ihandle* src_ih, Ihandle* dst_ih);
public procedure IupCopyClassAttributes(atom src_ih, atom dst_ih)
    c_proc(xIupCopyClassAttributes, {src_ih,dst_ih})
end procedure

--void IupSetClassDefaultAttribute(const char* classname, const char *name, const char* v);
public procedure IupSetClassDefaultAttribute(object classname = NULL, object name = NULL, object v = NULL)
    if sequence(classname) then classname = allocate_string(classname) end if
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupSetClassDefaultAttribute, {classname,name,v})
end procedure

--int IupClassMatch(Ihandle* ih, const char* classname);
public function IupClassMatch(atom ih, object classname = NULL)
    if sequence(classname) then classname = allocate_string(classname) end if
    atom result = c_func(xIupClassMatch, {ih,classname})
    return result
end function

--Ihandle* IupCreatev(const char *classname, void* *params);
public function IupCreate(object classname = NULL, sequence params = {})
    if sequence(classname) then classname = allocate_string(classname) end if
    atom p_params = allocate_pointer_array(params)
    atom result = c_func(xIupCreate, {classname,p_params})
    free(p_params)
    return result
end function

/************************************************************************/
/*                        Elements                                      */
/************************************************************************/
--Ihandle* IupFill(void);
public function IupFill()
atom result = c_func(xIupFill, {})
    return result
end function

--Ihandle* IupRadio(Ihandle* child);
public function IupRadio(atom child)
atom result = c_func(xIupRadio, {child})
    return result
end function

--Ihandle* IupVboxv(Ihandle* *children);
public function IupVbox(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupVbox, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupZboxv(Ihandle* *children);
public function IupZbox(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupZbox, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupHboxv(Ihandle* *children);
public function IupHbox(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupHbox, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupNormalizerv(Ihandle* *ih_list);
public function IupNormalizer(sequence ih_list = {})
atom p_ih_list = allocate_pointer_array(ih_list)
atom result = c_func(xIupNormalizer, {p_ih_list})
    free(p_ih_list)
    return result
end function

--Ihandle* IupCboxv(Ihandle* *children);
public function IupCbox(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupCbox, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupSbox(Ihandle *child);
public function IupSbox(atom child)
atom result = c_func(xIupSbox, {child})
    return result
end function

--Ihandle* IupSplit(Ihandle* child1, Ihandle* child2);
public function IupSplit(atom child1, atom child2)
atom result = c_func(xIupSplit, {child1,child2})
    return result
end function

--Ihandle* IupScrollBox(Ihandle* child);
public function IupScrollBox(atom child)
atom result = c_func(xIupScrollBox, {child})
    return result
end function

--Ihandle* IupGridBoxv(Ihandle **children);
public function IupGridBox(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupGridBox, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupExpander(Ihandle *child);
public function IupExpander(atom child)
atom result = c_func(xIupExpander, {child})
    return result
end function

--Ihandle* IupDetachBox(Ihandle *child);
public function IupDetachBox(atom child)
atom result = c_func(xIupDetachBox, {child})
    return result
end function

--Ihandle* IupBackgroundBox(Ihandle *child);
public function IupBackgroundBox(atom child)
atom result = c_func(xIupBackgroundBox, {child})
    return result
end function

--Ihandle* IupFrame(Ihandle* child);
public function IupFrame(atom child)
atom result = c_func(xIupFrame, {child})
    return result
end function

--Ihandle* IupImage(int width, int height, const unsigned char *pixmap);
public function IupImage(atom width, atom height, atom pixmap)
atom result = c_func(xIupImage, {width,height,pixmap})
    return result
end function

--Ihandle* IupImageRGB(int width, int height, const unsigned char *pixmap);
public function IupImageRGB(atom width, atom height, atom pixmap)
atom result = c_func(xIupImageRGB, {width,height,pixmap})
    return result
end function

--Ihandle* IupImageRGBA(int width, int height, const unsigned char *pixmap);
public function IupImageRGBA(atom width, atom height, atom pixmap)
atom result = c_func(xIupImageRGBA, {width,height,pixmap})
    return result
end function

--Ihandle* IupItem(const char* title, const char* action);
public function IupItem(object title = NULL, object action = NULL)
    if sequence(title) then title = allocate_string(title) end if
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupItem, {title,action})
    return result
end function

--Ihandle* IupSubmenu(const char* title, Ihandle* child);
public function IupSubmenu(object title, atom child)
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupSubmenu, {title,child})
    return result
end function

--Ihandle* IupSeparator(void);
public function IupSeparator()
atom result = c_func(xIupSeparator, {})
    return result
end function

--Ihandle* IupMenuv(Ihandle* *children);
public function IupMenu(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupMenu, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupButton(const char* title, const char* action);
public function IupButton(object title = NULL, object action = NULL)
    if sequence(title) then title = allocate_string(title) end if
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupButton, {title,action})
    return result
end function

--Ihandle* IupCanvas(const char* action);
public function IupCanvas(object action = NULL)
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupCanvas, {action})
    return result
end function

--Ihandle* IupDialog(Ihandle* child);
public function IupDialog(atom child)
atom result = c_func(xIupDialog, {child})
    return result
end function

--Ihandle* IupUser(void);
public function IupUser()
atom result = c_func(xIupUser, {})
    return result
end function

--Ihandle* IupLabel(const char* title);
public function IupLabel(object title = NULL)
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupLabel, {title})
    return result
end function

--Ihandle* IupList(const char* action);
public function IupList(object action = NULL)
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupList, {action})
    return result
end function

--Ihandle* IupText(const char* action);
public function IupText(object action = NULL)
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupText, {action})
    return result
end function

--Ihandle* IupMultiLine(const char* action);
public function IupMultiLine(object action = NULL)
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupMultiLine, {action})
    return result
end function

--Ihandle* IupToggle(const char* title, const char* action);
public function IupToggle(object title = NULL, object action = NULL)
    if sequence(title) then title = allocate_string(title) end if
    if sequence(action) then action = allocate_string(action) end if
    atom result = c_func(xIupToggle, {title,action})
    return result
end function

--Ihandle* IupTimer(void);
public function IupTimer()
atom result = c_func(xIupTimer, {})
    return result
end function

--Ihandle* IupClipboard(void);
public function IupClipboard()
atom result = c_func(xIupClipboard, {})
    return result
end function

--Ihandle* IupProgressBar(void);
public function IupProgressBar()
atom result = c_func(xIupProgressBar, {})
    return result
end function

--Ihandle* IupVal(const char *type);
public function IupVal(object xtype = NULL)
    if sequence(xtype) then xtype = allocate_string(xtype) end if
    atom result = c_func(xIupVal, {xtype})
    return result
end function

--Ihandle* IupTabsv(Ihandle* *children);
public function IupTabs(sequence children = {})
atom p_children = allocate_pointer_array(children)
atom result = c_func(xIupTabs, {p_children})
    free(p_children)
    return result
end function

--Ihandle* IupTree(void);
public function IupTree()
atom result = c_func(xIupTree, {})
    return result
end function

--Ihandle* IupLink(const char* url, const char* title);
public function IupLink(object url = NULL, object title = NULL)
    if sequence(url) then url = allocate_string(url) end if
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupLink, {url,title})
    return result
end function

--Ihandle* IupFlatButton(const char* title);
public function IupFlatButton(object title = NULL)
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupFlatButton, {title})
    return result
end function

/* Old controls, use SPIN attribute of IupText */
--Ihandle* IupSpin(void);
public function IupSpin()
atom result = c_func(xIupSpin, {})
    return result
end function

--Ihandle* IupSpinbox(Ihandle* child);
public function IupSpinbox(atom child)
atom result = c_func(xIupSpinbox, {child})
    return result
end function

/************************************************************************/
/*                      Utilities                                       */
/************************************************************************/
/* IupImage utility */
--int IupSaveImageAsText(Ihandle* ih, const char* file_name, const char* format, const char* name);
public function IupSaveImageAsText(atom ih, object file_name = NULL, object format = NULL, object name = NULL)
    if sequence(file_name) then file_name = allocate_string(file_name) end if
    if sequence(format) then format = allocate_string(format) end if
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupSaveImageAsText, {ih,file_name,format,name})
    return result
end function

/* IupText and IupScintilla utilities */
--void IupTextConvertLinColToPos(Ihandle* ih, int lin, int col, int *pos);
--public procedure IupTextConvertLinColToPos(atom ih, atom lin, atom col, atom pos)
--  c_proc(xIupTextConvertLinColToPos, {ih,lin,col,pos})
--end procedure
global function IupTextConvertLinColToPos(atom ih, atom lin, atom col)
atom pPos = allocate(8,1)
    c_proc(xIupTextConvertLinColToPos, {ih,lin,col,pPos})
    return peek4s(pPos)
end function

--void IupTextConvertPosToLinCol(Ihandle* ih, int pos, int *lin, int *col);
--public procedure IupTextConvertPosToLinCol(atom ih, atom pos, atom lin, atom col)
--  c_proc(xIupTextConvertPosToLinCol, {ih,pos,lin,col})
--end procedure
global function IupTextConvertPosToLinCol(atom ih, atom pos)
atom pLineCol = allocate(16,1)
    c_proc(xIupTextConvertPosToLinCol, {ih,pos,pLineCol,pLineCol+4})
    return peek4s({pLineCol,2})
end function

/* IupText, IupList, IupTree, IupMatrix and IupScintilla utility */
--int IupConvertXYToPos(Ihandle* ih, int x, int y);
public function IupConvertXYToPos(atom ih, atom x, atom y)
atom result = c_func(xIupConvertXYToPos, {ih,x,y})
    return result
end function

/* OLD names, kept for backward compatibility, will never be removed. */
--void IupStoreGlobal(const char* name, const char* v);
public procedure IupStoreGlobal(object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreGlobal, {name,v})
end procedure

--void IupStoreAttribute(Ihandle* ih, const char* name, const char* v);
public procedure IupStoreAttribute(atom ih, object name = NULL, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreAttribute, {ih,name,v})
end procedure

--void IupStoreAttributeId(Ihandle *ih, const char* name, int id, const char *v);
public procedure IupStoreAttributeId(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreAttributeId, {ih,name,id,v})
end procedure

--void IupStoreAttributeId2(Ihandle* ih, const char* name, int lin, int col, const char* v);
public procedure IupStoreAttributeId2(atom ih, object name, atom lin, atom col, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreAttributeId2, {ih,name,lin,col,v})
end procedure

/* IupTree utilities */
--int IupTreeSetUserId(Ihandle* ih, int id, void* userid);
public function IupTreeSetUserId(atom ih, atom id, atom userid)
atom result = c_func(xIupTreeSetUserId, {ih,id,userid})
    return result
end function

--void* IupTreeGetUserId(Ihandle* ih, int id);
public function IupTreeGetUserId(atom ih, atom id)
atom result = c_func(xIupTreeGetUserId, {ih,id})
    return result
end function

--int IupTreeGetId(Ihandle* ih, void *userid);
public function IupTreeGetId(atom ih, atom userid)
atom result = c_func(xIupTreeGetId, {ih,userid})
    return result
end function

--void IupTreeSetAttributeHandle(Ihandle* ih, const char* name, int id, Ihandle* ih_named);
public procedure IupTreeSetAttributeHandle(atom ih, object name, atom id, atom ih_named)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupTreeSetAttributeHandle, {ih,name,id,ih_named})
end procedure

/* DEPRECATED IupTree utilities, use Iup*AttributeId functions. It will be removed in a future version.  */
--void IupTreeSetAttribute(Ihandle* ih, const char* name, int id, const char* v);
public procedure IupTreeSetAttribute(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupTreeSetAttribute, {ih,name,id,v})
end procedure

--void IupTreeStoreAttribute(Ihandle* ih, const char* name, int id, const char* v);
public procedure IupTreeStoreAttribute(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupTreeStoreAttribute, {ih,name,id,v})
end procedure

--char* IupTreeGetAttribute(Ihandle* ih, const char* name, int id);
public function IupTreeGetAttribute(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupTreeGetAttribute, {ih,name,id})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupTreeGetInt(Ihandle* ih, const char* name, int id);
public function IupTreeGetInt(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupTreeGetInt, {ih,name,id})
    return result
end function

--float IupTreeGetFloat(Ihandle* ih, const char* name, int id);
public function IupTreeGetFloat(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupTreeGetFloat, {ih,name,id})
    return result
end function

/* DEPRECATED callback management. It will be removed in a future version. */
--const char* IupGetActionName(void);
public function IupGetActionName()
atom result = c_func(xIupGetActionName, {})
    return result
end function

/* DEPRECATED font names. It will be removed in a future version.  */
--char* IupMapFont(const char *iupfont);
public function IupMapFont(object iupfont = NULL)
    if sequence(iupfont) then iupfont = allocate_string(iupfont) end if
    atom ptr = c_func(xIupMapFont, {iupfont})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupUnMapFont(const char *driverfont);
public function IupUnMapFont(object driverfont = NULL)
    if sequence(driverfont) then driverfont = allocate_string(driverfont) end if
    atom ptr = c_func(xIupUnMapFont, {driverfont})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

/************************************************************************/
/*                      Pre-defined dialogs                           */
/************************************************************************/
--Ihandle* IupFileDlg(void);
public function IupFileDlg()
atom result = c_func(xIupFileDlg, {})
    return result
end function

--Ihandle* IupMessageDlg(void);
public function IupMessageDlg()
atom result = c_func(xIupMessageDlg, {})
    return result
end function

--Ihandle* IupColorDlg(void);
public function IupColorDlg()
atom result = c_func(xIupColorDlg, {})
    return result
end function

--Ihandle* IupFontDlg(void);
public function IupFontDlg()
atom result = c_func(xIupFontDlg, {})
    return result
end function

--Ihandle* IupProgressDlg(void);
public function IupProgressDlg()
atom result = c_func(xIupProgressDlg, {})
    return result
end function

--int IupGetFile(char *arq);
public function IupGetFile(atom arq)
atom result = c_func(xIupGetFile, {arq})
    return result
end function

--void IupMessage(const char *title, const char *msg);
public procedure IupMessage(object title = NULL, object msg = NULL)
    if sequence(title) then title = allocate_string(title) end if
    if sequence(msg) then msg = allocate_string(msg) end if
    c_proc(xIupMessage, {title,msg})
end procedure

--int IupAlarm(const char *title, const char *msg, const char *b1, const char *b2, const char *b3);
public function IupAlarm(object title = NULL, object msg = NULL, object b1 = NULL, object b2 = NULL, object b3 = NULL)
    if sequence(title) then title = allocate_string(title) end if
    if sequence(msg) then msg = allocate_string(msg) end if
    if sequence(b1) then b1 = allocate_string(b1) end if
    if sequence(b2) then b2 = allocate_string(b2) end if
    if sequence(b3) then b3 = allocate_string(b3) end if
    atom result = c_func(xIupAlarm, {title,msg,b1,b2,b3})
    return result
end function

--int IupListDialog(int type, const char *title, int size, const char** list,  int op, int max_col, int max_lin, int* marks);
public function IupListDialog(atom xtype, object title, atom size, sequence list, atom op, atom max_col, atom max_lin, atom marks)
    if sequence(title) then title = allocate_string(title) end if
    atom p_list = allocate_string_pointer_array(list)
    atom result = c_func(xIupListDialog, {xtype,title,size,p_list,op,max_col,max_lin,marks})
    free(p_list)
    return result
end function

--int IupGetText(const char* title, char* text);
public function IupGetText(object title, atom text)
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupGetText, {title,text})
    return result
end function

--int IupGetColor(int x, int y, unsigned char* r, unsigned char* g, unsigned char* b);
public function IupGetColor(atom x, atom y)
atom rgb = allocate_data(sizeof(C_INT)*3, 1)
atom r = rgb+sizeof(C_INT)*0
atom g = rgb+sizeof(C_INT)*1
atom b = rgb+sizeof(C_INT)*2
atom result = c_func(xIupGetColor, {x,y,r,g,b})
    if result=1 then return peek({rgb, 3}) end if
    return {}
end function

--int IupGetParamv(const char* title, Iparamcb action, void* user_data, const char* format, int param_count, int param_extra, void** param_data);
public function IupGetParam(object title, atom action, atom user_data, object format, atom param_count, atom param_extra, sequence param_data = {})
    if sequence(title) then title = allocate_string(title) end if
    if sequence(format) then format = allocate_string(format) end if
    atom p_param_data = allocate_pointer_array(param_data)
    atom result = c_func(xIupGetParam, {title,action,user_data,format,param_count,param_extra,p_param_data})
    free(p_param_data)
    return result
end function

--Ihandle* IupParamf(const char* format);
public function IupParamf(object format = NULL)
    if sequence(format) then format = allocate_string(format) end if
    atom result = c_func(xIupParamf, {format})
    return result
end function

--Ihandle* IupParamBox(Ihandle* parent, Ihandle** params, int count);
public function IupParamBox(atom parent, atom params, atom count)
atom result = c_func(xIupParamBox, {parent,params,count})
    return result
end function

--Ihandle* IupLayoutDialog(Ihandle* dialog);
public function IupLayoutDialog(atom dialog)
atom result = c_func(xIupLayoutDialog, {dialog})
    return result
end function

--Ihandle* IupElementPropertiesDialog(Ihandle* elem);
public function IupElementPropertiesDialog(atom elem)
atom result = c_func(xIupElementPropertiesDialog, {elem})
    return result
end function

/************************************************************************/
/*                   Callback Return Values                             */
/************************************************************************/
public constant IUP_IGNORE = -1
public constant IUP_DEFAULT = -2
public constant IUP_CLOSE = -3
public constant IUP_CONTINUE = -4
/************************************************************************/
/*           IupPopup and IupShowXY Parameter Values                    */
/************************************************************************/
public constant IUP_CENTER = 0xFFFF /* 65535 */
public constant IUP_LEFT = 0xFFFE /* 65534 */
public constant IUP_RIGHT = 0xFFFD /* 65533 */
public constant IUP_MOUSEPOS = 0xFFFC /* 65532 */
public constant IUP_CURRENT = 0xFFFB /* 65531 */
public constant IUP_CENTERPARENT = 0xFFFA /* 65530 */
public constant IUP_TOP = IUP_LEFT
public constant IUP_BOTTOM = IUP_RIGHT
/************************************************************************/
/*               SHOW_CB Callback Values                                */
/************************************************************************/
public enum
        IUP_SHOW = 0,
        IUP_RESTORE,
        IUP_MINIMIZE,
        IUP_MAXIMIZE,
        IUP_HIDE,
$
/************************************************************************/
/*               SCROLL_CB Callback Values                              */
/************************************************************************/
public enum
        IUP_SBUP = 0,
        IUP_SBDN,
        IUP_SBPGUP,
        IUP_SBPGDN,
        IUP_SBPOSV,
        IUP_SBDRAGV,
        IUP_SBLEFT,
        IUP_SBRIGHT,
        IUP_SBPGLEFT,
        IUP_SBPGRIGHT,
        IUP_SBPOSH,
        IUP_SBDRAGH,
$
/************************************************************************/
/*               Mouse Button Values and Macros                         */
/************************************************************************/
public constant IUP_BUTTON1 = '1'
public constant IUP_BUTTON2 = '2'
public constant IUP_BUTTON3 = '3'
public constant IUP_BUTTON4 = '4'
public constant IUP_BUTTON5 = '5'
/* Old definitions for backward compatibility */
/************************************************************************/
/*                      Pre-Defined Masks                               */
/************************************************************************/
public constant IUP_MASK_FLOAT = "[+/-]?(/d+/.?/d*|/./d+)"
public constant IUP_MASK_UFLOAT = "(/d+/.?/d*|/./d+)"
public constant IUP_MASK_EFLOAT = "[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?"
public constant IUP_MASK_FLOATCOMMA = "[+/-]?(/d+/,?/d*|/,/d+)"
public constant IUP_MASK_UFLOATCOMMA = "(/d+/,?/d*|/,/d+)"
public constant IUP_MASK_INT = "[+/-]?/d+"
public constant IUP_MASK_UINT = "/d+"
/* Old definitions for backward compatibility */
public constant IUPMASK_FLOAT = IUP_MASK_FLOAT
public constant IUPMASK_UFLOAT = IUP_MASK_UFLOAT
public constant IUPMASK_EFLOAT = IUP_MASK_EFLOAT
public constant IUPMASK_INT = IUP_MASK_INT
public constant IUPMASK_UINT = IUP_MASK_UINT
/************************************************************************/
/*                   IupGetParam Callback situations                    */
/************************************************************************/
public constant IUP_GETPARAM_BUTTON1 = -1
public constant IUP_GETPARAM_INIT = -2
public constant IUP_GETPARAM_BUTTON2 = -3
public constant IUP_GETPARAM_BUTTON3 = -4
public constant IUP_GETPARAM_CLOSE = -5
public constant IUP_GETPARAM_OK = IUP_GETPARAM_BUTTON1
public constant IUP_GETPARAM_CANCEL = IUP_GETPARAM_BUTTON2
public constant IUP_GETPARAM_HELP = IUP_GETPARAM_BUTTON3
/************************************************************************/
/*                   Record Input Modes                                 */
/************************************************************************/
public enum
        IUP_RECBINARY = 0,
        IUP_RECTEXT,
$
/************************************************************************/
/*              Replacement for the WinMain in Windows,                 */
/*        this allows the application to start from "main".             */
/*        Used only for Watcom.                                         */
/************************************************************************/

atom zlib1 = open_dll({ "win32\\zlib1.dll", "??libiupim.so", "??libiupim.dylib" })
atom im = open_dll({ "win32\\im.dll", "??libiupim.so", "??libiupim.dylib" })
atom iupim = open_dll({ "win32\\iupim.dll", "libiupim.so" })

public constant -- function delcarations
        xIupLoadImage                     = define_c_func( iupim, "+IupLoadImage", {C_PTR}, C_PTR ),
        xIupSaveImage                     = define_c_func( iupim, "+IupSaveImage", {C_PTR,C_PTR,C_PTR}, C_INT ),
        xIupGetNativeHandleImage          = define_c_func( iupim, "+IupGetNativeHandleImage", {C_PTR}, C_PTR ),
        xIupGetImageNativeHandle          = define_c_func( iupim, "+IupGetImageNativeHandle", {C_PTR}, C_PTR ),
        xIupImageFromImImage              = define_c_func( iupim, "+IupImageFromImImage", {C_PTR}, C_PTR ),
$

--Ihandle* IupLoadImage(const char* file_name);
public function IupLoadImage(object file_name = NULL)
        if sequence(file_name) then file_name = allocate_string(file_name) end if
        atom result = c_func( xIupLoadImage, {file_name} )
        return result
end function

--int IupSaveImage(Ihandle* ih, const char* file_name, const char* format);
public function IupSaveImage(atom ih, object file_name = NULL, object format = NULL)
        if sequence(file_name) then file_name = allocate_string(file_name) end if
        if sequence(format) then format = allocate_string(format) end if
        atom result = c_func( xIupSaveImage, {ih,file_name,format} )
        return result
end function

--imImage* IupGetNativeHandleImage(void* handle);
public function IupGetNativeHandleImage(atom handle)
        atom result = c_func( xIupGetNativeHandleImage, {handle} )
        return result
end function

--void* IupGetImageNativeHandle(const imImage* image);
public function IupGetImageNativeHandle(atom image)
        atom result = c_func( xIupGetImageNativeHandle, {image} )
        return result
end function

--Ihandle* IupImageFromImImage(const imImage* image);
public function IupImageFromImImage(atom image)
        atom result = c_func( xIupImageFromImImage, {image} )
        return result
end function

--atom iup = open_dll({"bin\\iup.dll", "libiup.so"})

public constant -- function delcarations
        xIupConfig                        = define_c_func(iup, "+IupConfig", {}, C_PTR),
        xIupConfigLoad                    = define_c_func(iup, "+IupConfigLoad", {C_PTR}, C_INT),
        xIupConfigSave                    = define_c_func(iup, "+IupConfigSave", {C_PTR}, C_INT),
        xIupConfigSetVariableStr          = define_c_proc(iup, "+IupConfigSetVariableStr", {C_PTR,C_PTR,C_PTR,C_PTR}),
        xIupConfigSetVariableStrId        = define_c_proc(iup, "+IupConfigSetVariableStrId", {C_PTR,C_PTR,C_PTR,C_INT,C_PTR}),
        xIupConfigSetVariableInt          = define_c_proc(iup, "+IupConfigSetVariableInt", {C_PTR,C_PTR,C_PTR,C_INT}),
        xIupConfigSetVariableIntId        = define_c_proc(iup, "+IupConfigSetVariableIntId", {C_PTR,C_PTR,C_PTR,C_INT,C_INT}),
        xIupConfigSetVariableDouble       = define_c_proc(iup, "+IupConfigSetVariableDouble", {C_PTR,C_PTR,C_PTR,C_DOUBLE}),
        xIupConfigSetVariableDoubleId     = define_c_proc(iup, "+IupConfigSetVariableDoubleId", {C_PTR,C_PTR,C_PTR,C_INT,C_DOUBLE}),
        xIupConfigGetVariableStr          = define_c_func(iup, "+IupConfigGetVariableStr", {C_PTR,C_PTR,C_PTR}, C_PTR),
        xIupConfigGetVariableStrId        = define_c_func(iup, "+IupConfigGetVariableStrId", {C_PTR,C_PTR,C_PTR,C_INT}, C_PTR),
        xIupConfigGetVariableInt          = define_c_func(iup, "+IupConfigGetVariableInt", {C_PTR,C_PTR,C_PTR}, C_INT),
        xIupConfigGetVariableIntId        = define_c_func(iup, "+IupConfigGetVariableIntId", {C_PTR,C_PTR,C_PTR,C_INT}, C_INT),
        xIupConfigGetVariableDouble       = define_c_func(iup, "+IupConfigGetVariableDouble", {C_PTR,C_PTR,C_PTR}, C_DOUBLE),
        xIupConfigGetVariableDoubleId     = define_c_func(iup, "+IupConfigGetVariableDoubleId", {C_PTR,C_PTR,C_PTR,C_INT}, C_DOUBLE),
        xIupConfigGetVariableStrDef       = define_c_func(iup, "+IupConfigGetVariableStrDef", {C_PTR,C_PTR,C_PTR,C_PTR}, C_PTR),
        xIupConfigGetVariableStrIdDef     = define_c_func(iup, "+IupConfigGetVariableStrIdDef", {C_PTR,C_PTR,C_PTR,C_INT,C_PTR}, C_PTR),
        xIupConfigGetVariableIntDef       = define_c_func(iup, "+IupConfigGetVariableIntDef", {C_PTR,C_PTR,C_PTR,C_INT}, C_INT),
        xIupConfigGetVariableIntIdDef     = define_c_func(iup, "+IupConfigGetVariableIntIdDef", {C_PTR,C_PTR,C_PTR,C_INT,C_INT}, C_INT),
        xIupConfigGetVariableDoubleDef    = define_c_func(iup, "+IupConfigGetVariableDoubleDef", {C_PTR,C_PTR,C_PTR,C_DOUBLE}, C_DOUBLE),
        xIupConfigGetVariableDoubleIdDef  = define_c_func(iup, "+IupConfigGetVariableDoubleIdDef", {C_PTR,C_PTR,C_PTR,C_INT,C_DOUBLE}, C_DOUBLE),
        xIupConfigRecentInit              = define_c_proc(iup, "+IupConfigRecentInit", {C_PTR,C_PTR,C_PTR,C_INT}),
        xIupConfigRecentUpdate            = define_c_proc(iup, "+IupConfigRecentUpdate", {C_PTR,C_PTR}),
        xIupConfigDialogShow              = define_c_proc(iup, "+IupConfigDialogShow", {C_PTR,C_PTR,C_PTR}),
        xIupConfigDialogClosed            = define_c_proc(iup, "+IupConfigDialogClosed", {C_PTR,C_PTR,C_PTR}),
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
    if result=NULL then return "" end if
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
    return peek_string(result)
end function

--const char* IupConfigGetVariableStrIdDef(Ihandle* ih, const char* group, const char* key, int id, const char* def);
public function IupConfigGetVariableStrIdDef(atom ih, object group, object key, atom id, object def = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(def) then def = allocate_string(def,1) end if
    atom result = c_func(xIupConfigGetVariableStrIdDef, {ih,group,key,id,def})
    return peek_string(result)
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

atom iup_scintilla = open_dll({ "win32\\iup_scintilla.dll", "libiup_scintilla.so" })

public constant -- function delcarations
        xIupScintillaOpen                 = define_c_proc( iup_scintilla, "+IupScintillaOpen", {} ),
        xIupScintilla                     = define_c_func( iup_scintilla, "+IupScintilla", {}, C_PTR ),
        xIupScintillaSendMessage          = define_c_func( iup_scintilla, "+IupScintillaSendMessage", {C_PTR,C_UINT,C_PTR,C_PTR}, C_PTR ),
$

--void IupScintillaOpen(void);
public procedure IupScintillaOpen()
        c_proc( xIupScintillaOpen, {} )
end procedure

--Ihandle *IupScintilla(void);
public function IupScintilla()
        atom result = c_func( xIupScintilla, {} )
        return result
end function

--sptr_t IupScintillaSendMessage(Ihandle* ih, unsigned int iMessage, uptr_t wParam, sptr_t lParam);
public function IupScintillaSendMessage(atom ih, atom iMessage, atom wParam, atom lParam)
        atom result = c_func( xIupScintillaSendMessage, {ih,iMessage,wParam,lParam} )
        return result
end function


