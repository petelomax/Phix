-- xpm.ew
-- X Pixmap support routines for Win32Lib

with trace

include get.e		    -- value()
include wildcard.e	    -- upper case
include image.e 	    -- read_bitmap

integer errFlag 	    -- on error, sets code

global constant
    XPM_OK	    = 0,    -- no error
    XPM_ERR_FILE    = -1,   -- file related error
    XPM_ERR_COLORS  = -2,   -- too many colors in bitmap
    XPM_ERR_HEADER  = -3,   -- error in XPM header
    XPM_ERR_COLOR   = -4    -- couldn't translate color code

-- x color names
constant ColorNames = {
    { "grey0", #00, #00, #00 },
    { "black", #00, #00, #00 },
    { "gray0", #00, #00, #00 },
    { "navyblue", #00, #00, #80 },
    { "navy_blue", #00, #00, #80 },
    { "navy", #00, #00, #80 },
    { "blue4", #00, #00, #8B },
    { "sergblue", #00, #00, #B4 },
    { "medium_blue", #00, #00, #CD },
    { "blue3", #00, #00, #CD },
    { "mediumblue", #00, #00, #CD },
    { "blue2", #00, #00, #EE },
    { "blue1", #00, #00, #FF },
    { "blue", #00, #00, #FF },
    { "dark_green", #00, #64, #00 },
    { "darkgreen", #00, #64, #00 },
    { "deepskyblue4", #00, #68, #8B },
    { "turquoise4", #00, #86, #8B },
    { "green4", #00, #8B, #00 },
    { "springgreen4", #00, #8B, #45 },
    { "cyan4", #00, #8B, #8B },
    { "deepskyblue3", #00, #9A, #CD },
    { "deepskyblue2", #00, #B2, #EE },
    { "deep_sky_blue", #00, #BF, #FF },
    { "deepskyblue", #00, #BF, #FF },
    { "deepskyblue1", #00, #BF, #FF },
    { "turquoise3", #00, #C5, #CD },
    { "green3", #00, #CD, #00 },
    { "springgreen3", #00, #CD, #66 },
    { "cyan3", #00, #CD, #CD },
    { "dark_turquoise", #00, #CE, #D1 },
    { "darkturquoise", #00, #CE, #D1 },
    { "turquoise2", #00, #E5, #EE },
    { "green2", #00, #EE, #00 },
    { "springgreen2", #00, #EE, #76 },
    { "cyan2", #00, #EE, #EE },
    { "turquoise1", #00, #F5, #FF },
    { "medium_spring_green", #00, #FA, #9A },
    { "mediumspringgreen", #00, #FA, #9A },
    { "green1", #00, #FF, #00 },
    { "green", #00, #FF, #00 },
    { "springgreen1", #00, #FF, #7F },
    { "spring_green", #00, #FF, #7F },
    { "springgreen", #00, #FF, #7F },
    { "cyan", #00, #FF, #FF },
    { "cyan1", #00, #FF, #FF },
    { "grey1", #03, #03, #03 },
    { "gray1", #03, #03, #03 },
    { "gray2", #05, #05, #05 },
    { "grey2", #05, #05, #05 },
    { "gray3", #08, #08, #08 },
    { "grey3", #08, #08, #08 },
    { "gray4", #0A, #0A, #0A },
    { "grey4", #0A, #0A, #0A },
    { "gray5", #0D, #0D, #0D },
    { "grey5", #0D, #0D, #0D },
    { "grey6", #0F, #0F, #0F },
    { "gray6", #0F, #0F, #0F },
    { "dodgerblue4", #10, #4E, #8B },
    { "gray7", #12, #12, #12 },
    { "grey7", #12, #12, #12 },
    { "gray8", #14, #14, #14 },
    { "grey8", #14, #14, #14 },
    { "grey9", #17, #17, #17 },
    { "gray9", #17, #17, #17 },
    { "dodgerblue3", #18, #74, #CD },
    { "midnightblue", #19, #19, #70 },
    { "midnight_blue", #19, #19, #70 },
    { "grey10", #1A, #1A, #1A },
    { "gray10", #1A, #1A, #1A },
    { "grey11", #1C, #1C, #1C },
    { "gray11", #1C, #1C, #1C },
    { "dodgerblue2", #1C, #86, #EE },
    { "dodgerblue", #1E, #90, #FF },
    { "dodger_blue", #1E, #90, #FF },
    { "dodgerblue1", #1E, #90, #FF },
    { "grey12", #1F, #1F, #1F },
    { "gray12", #1F, #1F, #1F },
    { "lightseagreen", #20, #B2, #AA },
    { "light_sea_green", #20, #B2, #AA },
    { "gray13", #21, #21, #21 },
    { "grey13", #21, #21, #21 },
    { "forest_green", #22, #8B, #22 },
    { "forestgreen", #22, #8B, #22 },
    { "gray14", #24, #24, #24 },
    { "grey14", #24, #24, #24 },
    { "gray15", #26, #26, #26 },
    { "grey15", #26, #26, #26 },
    { "royalblue4", #27, #40, #8B },
    { "grey16", #29, #29, #29 },
    { "gray16", #29, #29, #29 },
    { "grey17", #2B, #2B, #2B },
    { "gray17", #2B, #2B, #2B },
    { "grey18", #2E, #2E, #2E },
    { "gray18", #2E, #2E, #2E },
    { "seagreen4", #2E, #8B, #57 },
    { "seagreen", #2E, #8B, #57 },
    { "sea_green", #2E, #8B, #57 },
    { "dark_slate_grey", #2F, #4F, #4F },
    { "dark_slate_gray", #2F, #4F, #4F },
    { "darkslategray", #2F, #4F, #4F },
    { "darkslategrey", #2F, #4F, #4F },
    { "grey19", #30, #30, #30 },
    { "gray19", #30, #30, #30 },
    { "lime_green", #32, #CD, #32 },
    { "limegreen", #32, #CD, #32 },
    { "gray20", #33, #33, #33 },
    { "grey20", #33, #33, #33 },
    { "grey21", #36, #36, #36 },
    { "gray21", #36, #36, #36 },
    { "steelblue4", #36, #64, #8B },
    { "gray22", #38, #38, #38 },
    { "grey22", #38, #38, #38 },
    { "royalblue3", #3A, #5F, #CD },
    { "gray23", #3B, #3B, #3B },
    { "grey23", #3B, #3B, #3B },
    { "medium_sea_green", #3C, #B3, #71 },
    { "mediumseagreen", #3C, #B3, #71 },
    { "grey24", #3D, #3D, #3D },
    { "gray24", #3D, #3D, #3D },
    { "grey25", #40, #40, #40 },
    { "gray25", #40, #40, #40 },
    { "turquoise", #40, #E0, #D0 },
    { "royalblue", #41, #69, #E1 },
    { "royal_blue", #41, #69, #E1 },
    { "gray26", #42, #42, #42 },
    { "grey26", #42, #42, #42 },
    { "royalblue2", #43, #6E, #EE },
    { "seagreen3", #43, #CD, #80 },
    { "grey27", #45, #45, #45 },
    { "gray27", #45, #45, #45 },
    { "chartreuse4", #45, #8B, #00 },
    { "aquamarine4", #45, #8B, #74 },
    { "steelblue", #46, #82, #B4 },
    { "steel_blue", #46, #82, #B4 },
    { "slateblue4", #47, #3C, #8B },
    { "grey28", #47, #47, #47 },
    { "gray28", #47, #47, #47 },
    { "dark_slate_blue", #48, #3D, #8B },
    { "darkslateblue", #48, #3D, #8B },
    { "royalblue1", #48, #76, #FF },
    { "mediumturquoise", #48, #D1, #CC },
    { "medium_turquoise", #48, #D1, #CC },
    { "gray29", #4A, #4A, #4A },
    { "grey29", #4A, #4A, #4A },
    { "skyblue4", #4A, #70, #8B },
    { "grey30", #4D, #4D, #4D },
    { "gray30", #4D, #4D, #4D },
    { "seagreen2", #4E, #EE, #94 },
    { "gray31", #4F, #4F, #4F },
    { "grey31", #4F, #4F, #4F },
    { "steelblue3", #4F, #94, #CD },
    { "gray32", #52, #52, #52 },
    { "grey32", #52, #52, #52 },
    { "darkslategray4", #52, #8B, #8B },
    { "cadetblue4", #53, #86, #8B },
    { "gray33", #54, #54, #54 },
    { "grey33", #54, #54, #54 },
    { "palegreen4", #54, #8B, #54 },
    { "seagreen1", #54, #FF, #9F },
    { "purple4", #55, #1A, #8B },
    { "dark_olive_green", #55, #6B, #2F },
    { "darkolivegreen", #55, #6B, #2F },
    { "grey34", #57, #57, #57 },
    { "gray34", #57, #57, #57 },
    { "grey35", #59, #59, #59 },
    { "gray35", #59, #59, #59 },
    { "grey36", #5C, #5C, #5C },
    { "gray36", #5C, #5C, #5C },
    { "steelblue2", #5C, #AC, #EE },
    { "mediumpurple4", #5D, #47, #8B },
    { "gray37", #5E, #5E, #5E },
    { "grey37", #5E, #5E, #5E },
    { "cadetblue", #5F, #9E, #A0 },
    { "cadet_blue", #5F, #9E, #A0 },
    { "lightskyblue4", #60, #7B, #8B },
    { "timgray", #61, #5C, #5C },
    { "gray38", #61, #61, #61 },
    { "grey38", #61, #61, #61 },
    { "grey39", #63, #63, #63 },
    { "gray39", #63, #63, #63 },
    { "steelblue1", #63, #B8, #FF },
    { "cornflowerblue", #64, #95, #ED },
    { "cornflower_blue", #64, #95, #ED },
    { "grey40", #66, #66, #66 },
    { "gray40", #66, #66, #66 },
    { "paleturquoise4", #66, #8B, #8B },
    { "chartreuse3", #66, #CD, #00 },
    { "mediumaquamarine", #66, #CD, #AA },
    { "medium_aquamarine", #66, #CD, #AA },
    { "aquamarine3", #66, #CD, #AA },
    { "darkorchid4", #68, #22, #8B },
    { "lightblue4", #68, #83, #8B },
    { "slateblue3", #69, #59, #CD },
    { "dimgray", #69, #69, #69 },
    { "dimgrey", #69, #69, #69 },
    { "gray41", #69, #69, #69 },
    { "dim_gray", #69, #69, #69 },
    { "grey41", #69, #69, #69 },
    { "dim_grey", #69, #69, #69 },
    { "olivedrab4", #69, #8B, #22 },
    { "darkseagreen4", #69, #8B, #69 },
    { "slateblue", #6A, #5A, #CD },
    { "slate_blue", #6A, #5A, #CD },
    { "grey42", #6B, #6B, #6B },
    { "gray42", #6B, #6B, #6B },
    { "olive_drab", #6B, #8E, #23 },
    { "olivedrab", #6B, #8E, #23 },
    { "slategray4", #6C, #7B, #8B },
    { "skyblue3", #6C, #A6, #CD },
    { "grey43", #6E, #6E, #6E },
    { "gray43", #6E, #6E, #6E },
    { "lightsteelblue4", #6E, #7B, #8B },
    { "darkolivegreen4", #6E, #8B, #3D },
    { "rosegray40", #70, #66, #66 },
    { "gray44", #70, #70, #70 },
    { "grey44", #70, #70, #70 },
    { "slate_grey", #70, #80, #90 },
    { "slate_gray", #70, #80, #90 },
    { "slategray", #70, #80, #90 },
    { "slategrey", #70, #80, #90 },
    { "gray45", #73, #73, #73 },
    { "grey45", #73, #73, #73 },
    { "gray46", #75, #75, #75 },
    { "grey46", #75, #75, #75 },
    { "chartreuse2", #76, #EE, #00 },
    { "aquamarine2", #76, #EE, #C6 },
    { "lightslategrey", #77, #88, #99 },
    { "light_slate_gray", #77, #88, #99 },
    { "lightslategray", #77, #88, #99 },
    { "light_slate_grey", #77, #88, #99 },
    { "grey47", #78, #78, #78 },
    { "gray47", #78, #78, #78 },
    { "darkslategray3", #79, #CD, #CD },
    { "mediumorchid4", #7A, #37, #8B },
    { "slateblue2", #7A, #67, #EE },
    { "grey48", #7A, #7A, #7A },
    { "gray48", #7A, #7A, #7A },
    { "lightcyan4", #7A, #8B, #8B },
    { "cadetblue3", #7A, #C5, #CD },
    { "mediumslateblue", #7B, #68, #EE },
    { "medium_slate_blue", #7B, #68, #EE },
    { "palegreen3", #7C, #CD, #7C },
    { "lawngreen", #7C, #FC, #00 },
    { "lawn_green", #7C, #FC, #00 },
    { "purple3", #7D, #26, #CD },
    { "grey49", #7D, #7D, #7D },
    { "gray49", #7D, #7D, #7D },
    { "skyblue2", #7E, #C0, #EE },
    { "grey50", #7F, #7F, #7F },
    { "gray50", #7F, #7F, #7F },
    { "chartreuse1", #7F, #FF, #00 },
    { "chartreuse", #7F, #FF, #00 },
    { "aquamarine1", #7F, #FF, #D4 },
    { "aquamarine", #7F, #FF, #D4 },
    { "gray51", #82, #82, #82 },
    { "grey51", #82, #82, #82 },
    { "slateblue1", #83, #6F, #FF },
    { "honeydew4", #83, #8B, #83 },
    { "azure4", #83, #8B, #8B },
    { "lightslateblue", #84, #70, #FF },
    { "light_slate_blue", #84, #70, #FF },
    { "gray52", #85, #85, #85 },
    { "grey52", #85, #85, #85 },
    { "grey53", #87, #87, #87 },
    { "gray53", #87, #87, #87 },
    { "sky_blue", #87, #CE, #EB },
    { "skyblue", #87, #CE, #EB },
    { "lightskyblue", #87, #CE, #FA },
    { "light_sky_blue", #87, #CE, #FA },
    { "skyblue1", #87, #CE, #FF },
    { "mediumpurple3", #89, #68, #CD },
    { "blue_violet", #8A, #2B, #E2 },
    { "blueviolet", #8A, #2B, #E2 },
    { "gray54", #8A, #8A, #8A },
    { "grey54", #8A, #8A, #8A },
    { "red4", #8B, #00, #00 },
    { "magenta4", #8B, #00, #8B },
    { "deeppink4", #8B, #0A, #50 },
    { "firebrick4", #8B, #1A, #1A },
    { "maroon4", #8B, #1C, #62 },
    { "violetred4", #8B, #22, #52 },
    { "brown4", #8B, #23, #23 },
    { "orangered4", #8B, #25, #00 },
    { "tomato4", #8B, #36, #26 },
    { "indianred4", #8B, #3A, #3A },
    { "hotpink4", #8B, #3A, #62 },
    { "coral4", #8B, #3E, #2F },
    { "darkorange4", #8B, #45, #00 },
    { "saddle_brown", #8B, #45, #13 },
    { "saddlebrown", #8B, #45, #13 },
    { "chocolate4", #8B, #45, #13 },
    { "sienna4", #8B, #47, #26 },
    { "palevioletred4", #8B, #47, #5D },
    { "orchid4", #8B, #47, #89 },
    { "salmon4", #8B, #4C, #39 },
    { "lightsalmon4", #8B, #57, #42 },
    { "orange4", #8B, #5A, #00 },
    { "tan4", #8B, #5A, #2B },
    { "lightpink4", #8B, #5F, #65 },
    { "pink4", #8B, #63, #6C },
    { "darkgoldenrod4", #8B, #65, #08 },
    { "plum4", #8B, #66, #8B },
    { "goldenrod4", #8B, #69, #14 },
    { "rosybrown4", #8B, #69, #69 },
    { "burlywood4", #8B, #73, #55 },
    { "gold4", #8B, #75, #00 },
    { "peachpuff4", #8B, #77, #65 },
    { "navajowhite4", #8B, #79, #5E },
    { "thistle4", #8B, #7B, #8B },
    { "bisque4", #8B, #7D, #6B },
    { "mistyrose4", #8B, #7D, #7B },
    { "wheat4", #8B, #7E, #66 },
    { "lightgoldenrod4", #8B, #81, #4C },
    { "antiquewhite4", #8B, #83, #78 },
    { "lavenderblush4", #8B, #83, #86 },
    { "khaki4", #8B, #86, #4E },
    { "seashell4", #8B, #86, #82 },
    { "cornsilk4", #8B, #88, #78 },
    { "lemonchiffon4", #8B, #89, #70 },
    { "snow4", #8B, #89, #89 },
    { "yellow4", #8B, #8B, #00 },
    { "lightyellow4", #8B, #8B, #7A },
    { "ivory4", #8B, #8B, #83 },
    { "grey55", #8C, #8C, #8C },
    { "gray55", #8C, #8C, #8C },
    { "lightskyblue3", #8D, #B6, #CD },
    { "darkslategray2", #8D, #EE, #EE },
    { "cadetblue2", #8E, #E5, #EE },
    { "gray56", #8F, #8F, #8F },
    { "grey56", #8F, #8F, #8F },
    { "darkseagreen", #8F, #BC, #8F },
    { "dark_sea_green", #8F, #BC, #8F },
    { "palegreen2", #90, #EE, #90 },
    { "purple2", #91, #2C, #EE },
    { "gray57", #91, #91, #91 },
    { "grey57", #91, #91, #91 },
    { "medium_purple", #93, #70, #DB },
    { "mediumpurple", #93, #70, #DB },
    { "dark_violet", #94, #00, #D3 },
    { "darkviolet", #94, #00, #D3 },
    { "gray58", #94, #94, #94 },
    { "grey58", #94, #94, #94 },
    { "grey59", #96, #96, #96 },
    { "gray59", #96, #96, #96 },
    { "paleturquoise3", #96, #CD, #CD },
    { "darkslategray1", #97, #FF, #FF },
    { "cadetblue1", #98, #F5, #FF },
    { "palegreen", #98, #FB, #98 },
    { "pale_green", #98, #FB, #98 },
    { "dark_orchid", #99, #32, #CC },
    { "darkorchid", #99, #32, #CC },
    { "gray60", #99, #99, #99 },
    { "grey60", #99, #99, #99 },
    { "darkorchid3", #9A, #32, #CD },
    { "lightblue3", #9A, #C0, #CD },
    { "olivedrab3", #9A, #CD, #32 },
    { "yellowgreen", #9A, #CD, #32 },
    { "yellow_green", #9A, #CD, #32 },
    { "palegreen1", #9A, #FF, #9A },
    { "purple1", #9B, #30, #FF },
    { "darkseagreen3", #9B, #CD, #9B },
    { "grey61", #9C, #9C, #9C },
    { "gray61", #9C, #9C, #9C },
    { "grey62", #9E, #9E, #9E },
    { "gray62", #9E, #9E, #9E },
    { "mediumpurple2", #9F, #79, #EE },
    { "slategray3", #9F, #B6, #CD },
    { "purple", #A0, #20, #F0 },
    { "sienna", #A0, #52, #2D },
    { "grey63", #A1, #A1, #A1 },
    { "gray63", #A1, #A1, #A1 },
    { "lightsteelblue3", #A2, #B5, #CD },
    { "darkolivegreen3", #A2, #CD, #5A },
    { "gray64", #A3, #A3, #A3 },
    { "grey64", #A3, #A3, #A3 },
    { "lightskyblue2", #A4, #D3, #EE },
    { "brown", #A5, #2A, #2A },
    { "gray65", #A6, #A6, #A6 },
    { "grey65", #A6, #A6, #A6 },
    { "gray66", #A8, #A8, #A8 },
    { "grey66", #A8, #A8, #A8 },
    { "mediumpurple1", #AB, #82, #FF },
    { "gray67", #AB, #AB, #AB },
    { "grey67", #AB, #AB, #AB },
    { "grey68", #AD, #AD, #AD },
    { "gray68", #AD, #AD, #AD },
    { "lightblue", #AD, #D8, #E6 },
    { "light_blue", #AD, #D8, #E6 },
    { "green_yellow", #AD, #FF, #2F },
    { "greenyellow", #AD, #FF, #2F },
    { "paleturquoise2", #AE, #EE, #EE },
    { "paleturquoise", #AF, #EE, #EE },
    { "pale_turquoise", #AF, #EE, #EE },
    { "maroon", #B0, #30, #60 },
    { "grey69", #B0, #B0, #B0 },
    { "gray69", #B0, #B0, #B0 },
    { "light_steel_blue", #B0, #C4, #DE },
    { "lightsteelblue", #B0, #C4, #DE },
    { "powder_blue", #B0, #E0, #E6 },
    { "powderblue", #B0, #E0, #E6 },
    { "lightskyblue1", #B0, #E2, #FF },
    { "firebrick", #B2, #22, #22 },
    { "darkorchid2", #B2, #3A, #EE },
    { "lightblue2", #B2, #DF, #EE },
    { "gray70", #B3, #B3, #B3 },
    { "grey70", #B3, #B3, #B3 },
    { "olivedrab2", #B3, #EE, #3A },
    { "mediumorchid3", #B4, #52, #CD },
    { "lightcyan3", #B4, #CD, #CD },
    { "darkseagreen2", #B4, #EE, #B4 },
    { "gray71", #B5, #B5, #B5 },
    { "grey71", #B5, #B5, #B5 },
    { "darkgoldenrod", #B8, #86, #0B },
    { "dark_goldenrod", #B8, #86, #0B },
    { "gray72", #B8, #B8, #B8 },
    { "grey72", #B8, #B8, #B8 },
    { "slategray2", #B9, #D3, #EE },
    { "medium_orchid", #BA, #55, #D3 },
    { "mediumorchid", #BA, #55, #D3 },
    { "grey73", #BA, #BA, #BA },
    { "gray73", #BA, #BA, #BA },
    { "paleturquoise1", #BB, #FF, #FF },
    { "rosy_brown", #BC, #8F, #8F },
    { "rosybrown", #BC, #8F, #8F },
    { "lightsteelblue2", #BC, #D2, #EE },
    { "darkolivegreen2", #BC, #EE, #68 },
    { "darkkhaki", #BD, #B7, #6B },
    { "dark_khaki", #BD, #B7, #6B },
    { "gray74", #BD, #BD, #BD },
    { "grey74", #BD, #BD, #BD },
    { "darkorchid1", #BF, #3E, #FF },
    { "grey75", #BF, #BF, #BF },
    { "gray75", #BF, #BF, #BF },
    { "lightblue1", #BF, #EF, #FF },
    { "grey", #C0, #C0, #C0 },
    { "gray", #C0, #C0, #C0 },
    { "olivedrab1", #C0, #FF, #3E },
    { "honeydew3", #C1, #CD, #C1 },
    { "azure3", #C1, #CD, #CD },
    { "darkseagreen1", #C1, #FF, #C1 },
    { "grey76", #C2, #C2, #C2 },
    { "gray76", #C2, #C2, #C2 },
    { "gray77", #C4, #C4, #C4 },
    { "grey77", #C4, #C4, #C4 },
    { "slategray1", #C6, #E2, #FF },
    { "mediumvioletred", #C7, #15, #85 },
    { "grey78", #C7, #C7, #C7 },
    { "gray78", #C7, #C7, #C7 },
    { "gray79", #C9, #C9, #C9 },
    { "grey79", #C9, #C9, #C9 },
    { "lightsteelblue1", #CA, #E1, #FF },
    { "darkolivegreen1", #CA, #FF, #70 },
    { "grey80", #CC, #CC, #CC },
    { "gray80", #CC, #CC, #CC },
    { "red3", #CD, #00, #00 },
    { "magenta3", #CD, #00, #CD },
    { "deeppink3", #CD, #10, #76 },
    { "firebrick3", #CD, #26, #26 },
    { "maroon3", #CD, #29, #90 },
    { "violetred3", #CD, #32, #78 },
    { "brown3", #CD, #33, #33 },
    { "orangered3", #CD, #37, #00 },
    { "tomato3", #CD, #4F, #39 },
    { "indianred3", #CD, #55, #55 },
    { "coral3", #CD, #5B, #45 },
    { "indianred", #CD, #5C, #5C },
    { "indian_red", #CD, #5C, #5C },
    { "hotpink3", #CD, #60, #90 },
    { "darkorange3", #CD, #66, #00 },
    { "chocolate3", #CD, #66, #1D },
    { "sienna3", #CD, #68, #39 },
    { "palevioletred3", #CD, #68, #89 },
    { "orchid3", #CD, #69, #C9 },
    { "salmon3", #CD, #70, #54 },
    { "lightsalmon3", #CD, #81, #62 },
    { "orange3", #CD, #85, #00 },
    { "peru", #CD, #85, #3F },
    { "tan3", #CD, #85, #3F },
    { "lightpink3", #CD, #8C, #95 },
    { "pink3", #CD, #91, #9E },
    { "darkgoldenrod3", #CD, #95, #0C },
    { "plum3", #CD, #96, #CD },
    { "goldenrod3", #CD, #9B, #1D },
    { "rosybrown3", #CD, #9B, #9B },
    { "burlywood3", #CD, #AA, #7D },
    { "gold3", #CD, #AD, #00 },
    { "peachpuff3", #CD, #AF, #95 },
    { "navajowhite3", #CD, #B3, #8B },
    { "thistle3", #CD, #B5, #CD },
    { "bisque3", #CD, #B7, #9E },
    { "mistyrose3", #CD, #B7, #B5 },
    { "wheat3", #CD, #BA, #96 },
    { "lightgoldenrod3", #CD, #BE, #70 },
    { "antiquewhite3", #CD, #C0, #B0 },
    { "lavenderblush3", #CD, #C1, #C5 },
    { "seashell3", #CD, #C5, #BF },
    { "khaki3", #CD, #C6, #73 },
    { "cornsilk3", #CD, #C8, #B1 },
    { "lemonchiffon3", #CD, #C9, #A5 },
    { "snow3", #CD, #C9, #C9 },
    { "yellow3", #CD, #CD, #00 },
    { "lightyellow3", #CD, #CD, #B4 },
    { "ivory3", #CD, #CD, #C1 },
    { "grey81", #CF, #CF, #CF },
    { "gray81", #CF, #CF, #CF },
    { "violet_red", #D0, #20, #90 },
    { "violetred", #D0, #20, #90 },
    { "mediumorchid2", #D1, #5F, #EE },
    { "grey82", #D1, #D1, #D1 },
    { "gray82", #D1, #D1, #D1 },
    { "lightcyan2", #D1, #EE, #EE },
    { "chocolate", #D2, #69, #1E },
    { "tan", #D2, #B4, #8C },
    { "light_grey", #D3, #D3, #D3 },
    { "lightgray", #D3, #D3, #D3 },
    { "lightgrey", #D3, #D3, #D3 },
    { "light_gray", #D3, #D3, #D3 },
    { "grey83", #D4, #D4, #D4 },
    { "gray83", #D4, #D4, #D4 },
    { "grey84", #D6, #D6, #D6 },
    { "gray84", #D6, #D6, #D6 },
    { "thistle", #D8, #BF, #D8 },
    { "gray85", #D9, #D9, #D9 },
    { "grey85", #D9, #D9, #D9 },
    { "orchid", #DA, #70, #D6 },
    { "goldenrod", #DA, #A5, #20 },
    { "pale_violet_red", #DB, #70, #93 },
    { "palevioletred", #DB, #70, #93 },
    { "gray86", #DB, #DB, #DB },
    { "grey86", #DB, #DB, #DB },
    { "gainsboro", #DC, #DC, #DC },
    { "plum", #DD, #A0, #DD },
    { "burlywood", #DE, #B8, #87 },
    { "gray87", #DE, #DE, #DE },
    { "grey87", #DE, #DE, #DE },
    { "mediumorchid1", #E0, #66, #FF },
    { "grey88", #E0, #E0, #E0 },
    { "gray88", #E0, #E0, #E0 },
    { "honeydew2", #E0, #EE, #E0 },
    { "azure2", #E0, #EE, #EE },
    { "light_cyan", #E0, #FF, #FF },
    { "lightcyan1", #E0, #FF, #FF },
    { "lightcyan", #E0, #FF, #FF },
    { "gray89", #E3, #E3, #E3 },
    { "grey89", #E3, #E3, #E3 },
    { "grey90", #E5, #E5, #E5 },
    { "gray90", #E5, #E5, #E5 },
    { "lavender", #E6, #E6, #FA },
    { "gray91", #E8, #E8, #E8 },
    { "grey91", #E8, #E8, #E8 },
    { "dark_salmon", #E9, #96, #7A },
    { "darksalmon", #E9, #96, #7A },
    { "gray92", #EB, #EB, #EB },
    { "grey92", #EB, #EB, #EB },
    { "grey93", #ED, #ED, #ED },
    { "gray93", #ED, #ED, #ED },
    { "red2", #EE, #00, #00 },
    { "magenta2", #EE, #00, #EE },
    { "deeppink2", #EE, #12, #89 },
    { "firebrick2", #EE, #2C, #2C },
    { "maroon2", #EE, #30, #A7 },
    { "violetred2", #EE, #3A, #8C },
    { "brown2", #EE, #3B, #3B },
    { "orangered2", #EE, #40, #00 },
    { "tomato2", #EE, #5C, #42 },
    { "indianred2", #EE, #63, #63 },
    { "coral2", #EE, #6A, #50 },
    { "hotpink2", #EE, #6A, #A7 },
    { "darkorange2", #EE, #76, #00 },
    { "chocolate2", #EE, #76, #21 },
    { "sienna2", #EE, #79, #42 },
    { "palevioletred2", #EE, #79, #9F },
    { "orchid2", #EE, #7A, #E9 },
    { "salmon2", #EE, #82, #62 },
    { "violet", #EE, #82, #EE },
    { "lightsalmon2", #EE, #95, #72 },
    { "orange2", #EE, #9A, #00 },
    { "tan2", #EE, #9A, #49 },
    { "lightpink2", #EE, #A2, #AD },
    { "pink2", #EE, #A9, #B8 },
    { "darkgoldenrod2", #EE, #AD, #0E },
    { "plum2", #EE, #AE, #EE },
    { "goldenrod2", #EE, #B4, #22 },
    { "rosybrown2", #EE, #B4, #B4 },
    { "burlywood2", #EE, #C5, #91 },
    { "gold2", #EE, #C9, #00 },
    { "peachpuff2", #EE, #CB, #AD },
    { "navajowhite2", #EE, #CF, #A1 },
    { "thistle2", #EE, #D2, #EE },
    { "bisque2", #EE, #D5, #B7 },
    { "mistyrose2", #EE, #D5, #D2 },
    { "wheat2", #EE, #D8, #AE },
    { "lightgoldenrod2", #EE, #DC, #82 },
    { "light_goldenrod", #EE, #DD, #82 },
    { "lightgoldenrod", #EE, #DD, #82 },
    { "antiquewhite2", #EE, #DF, #CC },
    { "lavenderblush2", #EE, #E0, #E5 },
    { "seashell2", #EE, #E5, #DE },
    { "khaki2", #EE, #E6, #85 },
    { "pale_goldenrod", #EE, #E8, #AA },
    { "palegoldenrod", #EE, #E8, #AA },
    { "cornsilk2", #EE, #E8, #CD },
    { "lemonchiffon2", #EE, #E9, #BF },
    { "snow2", #EE, #E9, #E9 },
    { "yellow2", #EE, #EE, #00 },
    { "lightyellow2", #EE, #EE, #D1 },
    { "ivory2", #EE, #EE, #E0 },
    { "lightcoral", #F0, #80, #80 },
    { "light_coral", #F0, #80, #80 },
    { "khaki", #F0, #E6, #8C },
    { "gray94", #F0, #F0, #F0 },
    { "grey94", #F0, #F0, #F0 },
    { "alice_blue", #F0, #F8, #FF },
    { "aliceblue", #F0, #F8, #FF },
    { "honeydew1", #F0, #FF, #F0 },
    { "honeydew", #F0, #FF, #F0 },
    { "azure", #F0, #FF, #FF },
    { "azure1", #F0, #FF, #FF },
    { "gray95", #F2, #F2, #F2 },
    { "grey95", #F2, #F2, #F2 },
    { "sandybrown", #F4, #A4, #60 },
    { "sandy_brown", #F4, #A4, #60 },
    { "wheat", #F5, #DE, #B3 },
    { "beige", #F5, #F5, #DC },
    { "whitesmoke", #F5, #F5, #F5 },
    { "grey96", #F5, #F5, #F5 },
    { "gray96", #F5, #F5, #F5 },
    { "white_smoke", #F5, #F5, #F5 },
    { "mintcream", #F5, #FF, #FA },
    { "mint_cream", #F5, #FF, #FA },
    { "grey97", #F7, #F7, #F7 },
    { "gray97", #F7, #F7, #F7 },
    { "ghost_white", #F8, #F8, #FF },
    { "ghostwhite", #F8, #F8, #FF },
    { "salmon", #FA, #80, #72 },
    { "antiquewhite", #FA, #EB, #D7 },
    { "antique_white", #FA, #EB, #D7 },
    { "linen", #FA, #F0, #E6 },
    { "light_goldenrod_yellow", #FA, #FA, #D2 },
    { "lightgoldenrodyellow", #FA, #FA, #D2 },
    { "gray98", #FA, #FA, #FA },
    { "grey98", #FA, #FA, #FA },
    { "gray99", #FC, #FC, #FC },
    { "grey99", #FC, #FC, #FC },
    { "old_lace", #FD, #F5, #E6 },
    { "oldlace", #FD, #F5, #E6 },
    { "red1", #FF, #00, #00 },
    { "red", #FF, #00, #00 },
    { "magenta1", #FF, #00, #FF },
    { "magenta", #FF, #00, #FF },
    { "deeppink1", #FF, #14, #93 },
    { "deeppink", #FF, #14, #93 },
    { "deep_pink", #FF, #14, #93 },
    { "firebrick1", #FF, #30, #30 },
    { "maroon1", #FF, #34, #B3 },
    { "violetred1", #FF, #3E, #96 },
    { "brown1", #FF, #40, #40 },
    { "orangered", #FF, #45, #00 },
    { "orangered1", #FF, #45, #00 },
    { "orange_red", #FF, #45, #00 },
    { "tomato1", #FF, #63, #47 },
    { "tomato", #FF, #63, #47 },
    { "hotpink", #FF, #69, #B4 },
    { "hot_pink", #FF, #69, #B4 },
    { "indianred1", #FF, #6A, #6A },
    { "hotpink1", #FF, #6E, #B4 },
    { "coral1", #FF, #72, #56 },
    { "darkorange1", #FF, #7F, #00 },
    { "chocolate1", #FF, #7F, #24 },
    { "coral", #FF, #7F, #50 },
    { "sienna1", #FF, #82, #47 },
    { "palevioletred1", #FF, #82, #AB },
    { "orchid1", #FF, #83, #FA },
    { "dark_orange", #FF, #8C, #00 },
    { "darkorange", #FF, #8C, #00 },
    { "salmon1", #FF, #8C, #69 },
    { "lightsalmon1", #FF, #A0, #7A },
    { "lightsalmon", #FF, #A0, #7A },
    { "light_salmon", #FF, #A0, #7A },
    { "orange", #FF, #A5, #00 },
    { "orange1", #FF, #A5, #00 },
    { "tan1", #FF, #A5, #4F },
    { "lightpink1", #FF, #AE, #B9 },
    { "pink1", #FF, #B5, #C5 },
    { "lightpink", #FF, #B6, #C1 },
    { "light_pink", #FF, #B6, #C1 },
    { "darkgoldenrod1", #FF, #B9, #0F },
    { "plum1", #FF, #BB, #FF },
    { "pink", #FF, #C0, #CB },
    { "goldenrod1", #FF, #C1, #25 },
    { "rosybrown1", #FF, #C1, #C1 },
    { "burlywood1", #FF, #D3, #9B },
    { "gold1", #FF, #D7, #00 },
    { "gold", #FF, #D7, #00 },
    { "peachpuff", #FF, #DA, #B9 },
    { "peachpuff1", #FF, #DA, #B9 },
    { "peach_puff", #FF, #DA, #B9 },
    { "navajowhite", #FF, #DE, #AD },
    { "navajo_white", #FF, #DE, #AD },
    { "navajowhite1", #FF, #DE, #AD },
    { "thistle1", #FF, #E1, #FF },
    { "moccasin", #FF, #E4, #B5 },
    { "bisque1", #FF, #E4, #C4 },
    { "bisque", #FF, #E4, #C4 },
    { "mistyrose", #FF, #E4, #E1 },
    { "misty_rose", #FF, #E4, #E1 },
    { "mistyrose1", #FF, #E4, #E1 },
    { "wheat1", #FF, #E7, #BA },
    { "blanched_almond", #FF, #EB, #CD },
    { "blanchedalmond", #FF, #EB, #CD },
    { "lightgoldenrod1", #FF, #EC, #8B },
    { "papayawhip", #FF, #EF, #D5 },
    { "papaya_whip", #FF, #EF, #D5 },
    { "antiquewhite1", #FF, #EF, #DB },
    { "lavenderblush1", #FF, #F0, #F5 },
    { "lavender_blush", #FF, #F0, #F5 },
    { "lavenderblush", #FF, #F0, #F5 },
    { "seashell1", #FF, #F5, #EE },
    { "seashell", #FF, #F5, #EE },
    { "khaki1", #FF, #F6, #8F },
    { "cornsilk", #FF, #F8, #DC },
    { "cornsilk1", #FF, #F8, #DC },
    { "lemon_chiffon", #FF, #FA, #CD },
    { "lemonchiffon1", #FF, #FA, #CD },
    { "lemonchiffon", #FF, #FA, #CD },
    { "floral_white", #FF, #FA, #F0 },
    { "floralwhite", #FF, #FA, #F0 },
    { "snow1", #FF, #FA, #FA },
    { "snow", #FF, #FA, #FA },
    { "yellow1", #FF, #FF, #00 },
    { "yellow", #FF, #FF, #00 },
    { "light_yellow", #FF, #FF, #E0 },
    { "lightyellow", #FF, #FF, #E0 },
    { "lightyellow1", #FF, #FF, #E0 },
    { "ivory1", #FF, #FF, #F0 },
    { "ivory", #FF, #FF, #F0 },
    { "grey100", #FF, #FF, #FF },
    { "white", #FF, #FF, #FF },
    { "gray100", #FF, #FF, #FF },
    { "gray100", #FF, #FF, #FF } }


constant XPM_CODES = 
    " .+@#$%&*=-;>,')!~{]^/(_:<[}|1234567890" &
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


------------------------------------------------------------------------------
global function createXpm( sequence outFile, sequence bmp, sequence pal )

    -- create an XPM file from bitmap data
    integer				 
	handle,     -- out file handle
	index,	    -- convenience counter
	colors,     -- count of colors used in XMP
	charsWide,  -- chars wide of encoding
	at	    -- index
    sequence
	code,	    -- reassignment of codes to palette indexes
	encode	    -- encoding of color codes
    
    
    -- build a set of 'used' flags for the palette
    code = repeat( 0, length( pal ) )
    
    -- scan through the bitmap, and
    colors = 0
    for i = 1 to length( bmp ) do
	for j = 1 to length( bmp[i] )  do
	
	    -- get palette index
	    index = bmp[i][j]+1
	    
	    -- already assigned an index?
	    if not code[ index ] then
	    
		-- increment index count
		colors = colors + 1	  
		
		-- assign an index		  
		code[ index ] = colors		      
		
	    end if
	end for
    end for

    -- need 1 or 2 byte codes?
    if colors > length( XPM_CODES ) then

	-- need to construct 2 byte codes
	charsWide = 2
	
	-- initialize sequence
	encode = repeat( 0, colors )

	-- index
	at = 1
	
	for i = 1 to length( XPM_CODES ) do
	    for j = 2 to length( XPM_CODES ) do

		-- build a two byte code	    
		encode[at] = XPM_CODES[i] & XPM_CODES[j]

		-- increment
		at += 1
		
		-- filled?
		if at > colors then
		    -- leave inner loop
		    exit
		end if
		
	    end for	
	    
	    -- filled?
	    if at > colors then
		-- leave outer loop
		exit
	    end if
	    
	end for

    else
    
	-- 1 byte coding
	charsWide = 1
	
	-- list of codes is subset of
	encode = XPM_CODES[1..colors]
	
    end if
    
    -- open the file
    handle = open( outFile, "w" )			
    if handle = -1 then
	-- error opening file
	return XPM_ERR_FILE
    end if

    -- header
    puts( handle, "/* XPM */\n" )
    puts( handle, "static char * xpm[] = {\n" )
    
    -- values: width, height, colors, code width
    printf( handle, "\"%d %d %d %d\",\n", 
	{length(bmp[1]), length(bmp), colors, charsWide} )
    
    -- the color tuples
    for i = 1 to length( code ) do
	-- color used?
	if code[i] then
	    -- create an entry
	    printf( handle, "\"%s        c #%02x%02x%02x\",\n", 
		{encode[code[i]]} & pal[i] )
	end if
	
    end for

    -- build the xmp data	     
    for i = 1 to length( bmp ) do
	-- "
	puts( handle, "\"" )
	for j = 1 to length( bmp[i] ) do       
	
	    -- get palette index
	    index = bmp[i][j]+1
	    
	    -- write the code out
	    puts( handle, encode[ code[index] ] )
	    
	end for
	
	-- end of line
	if i = length( bmp ) then
	    -- "};
	    puts( handle, "\"};\n" )
	else	  
	    -- ",
	    puts( handle, "\",\n" )
	end if		
	
    end for   

    -- close the file
    close( handle )		     
    
    -- return success
    return XPM_OK
    
end function


------------------------------------------------------------------------------
global function createXpmFromBmp( sequence inFile, sequence outFile )

    -- create an XPM file from a bitmap file
    object bmp
    
    -- read the bitmap in
    bmp = read_bitmap( inFile )
    if integer( bmp ) then 
	-- file error
	return XPM_ERR_FILE
    end if	     
    
    -- convert to xpm
    return createXpm( outFile, bmp[2], bmp[1] )

end function


------------------------------------------------------------------------------
function convertToWords( sequence s )
    -- convert s into space delimited words
    sequence word, words
    
    words = {}
    word = ""
    s = s & " "
    for i = 1 to length( s ) do
	if s[i] = ' ' then
	    if length( word ) then
		words = append( words, word )
		word = ""
	    end if
	else
	    word = word & s[i]
	end if
    end for
    
    return words
    
end function


------------------------------------------------------------------------------
function convertToNumber( sequence s )

    -- convert a string to a number
    -- this should only return positive numbers,
    -- -1 indicates error

    s = value( s )
    if s[1] != GET_SUCCESS then  
	-- flag error
	errFlag = 1
    end if
    
    return s[2]
    
end function	   



------------------------------------------------------------------------------
function convertToRGB( sequence s, sequence clear )
    
    -- convert a color code string into an {r,g,b} tuple
    integer r,g,b

    if s[1] = '#' then
	-- hex tuple: #rrggbb
	
	-- upper case
	s = upper( s )
	
	r = convertToNumber( s[1..3] )	      
	g = convertToNumber( "#" & s[4..5] )
	b = convertToNumber( "#" & s[6..7] )	    

	-- return tuple
	return {r,g,b}		  

    elsif equal( s, "None" ) then
    
	-- set color to clear
	return clear

    else
	-- set name to lower case
	s = lower( s )
    
	-- look up color
	for i = 1 to length( ColorNames ) do
	    -- match?
	    if equal( s, ColorNames[i][1] ) then
		-- return the colors
		return ColorNames[i][2..4]
	    end if	      
	end for
	
    end if
	
    -- unable to convert color
    errFlag = 1   
    
    return {0,0,0}
	
end function


------------------------------------------------------------------------------
function removeNth( integer i, sequence s )
    -- remove the nth element
    return s[1..i-1] & s[i+1..length(s)]
end function


------------------------------------------------------------------------------
function ltrim( sequence s )

    -- remove leading spaces
    integer stopped

    -- marks where non-space was found	  
    stopped = -1
    
    for i = 1 to length( s ) do
	if s[i] != ' ' then
	    stopped = i
	    exit
	end if
    end for
    
    if stopped = -1 then
	-- all spaces
	return ""
    else			    
	-- trim
	return s[stopped..length(s)]
    end if
	
end function


------------------------------------------------------------------------------
global function XpmToBmp( sequence xpm, sequence clearRGB )

    -- read an XPM, convert it into a standard Euphoria bitmap

    integer
	line,		-- current line in file
	at,		-- used to find positions in strings
	bitsWide,	-- width of the xpm
	bitsTall,	-- height of the xpm
	colors, 	-- colors in the xpm
	codeWide	-- width of the color code
    
    sequence		   
	colorCode,	-- text of color code
	colorRGB,	-- palette of tuples
	bits		-- bits of image
    
    object 
	data		-- generic bucket
    

    -- clear error flag
    errFlag = 0
    
    -- extract the values: "<wide> <tall> <colors> <bytes per color>"
    data = convertToWords( xpm[1] )	       
    bitsWide = convertToNumber( data[1] )
    bitsTall = convertToNumber( data[2] )
    colors   = convertToNumber( data[3] )
    codeWide = convertToNumber( data[4] )

    -- error?
    if errFlag then	     
	-- error in header
	return XPM_ERR_HEADER
    end if

    -- create a palatte and data
    colorCode  = repeat( "", colors )
    colorRGB   = repeat( {}, colors )

    -- start of data
    line = 2

    -- read the color data
    for i = 1 to colors do
    
	-- extract the color code    
	colorCode[i] = xpm[line][1..codeWide]

	-- get a line
	data = xpm[line]

	-- seek to the color
	data = data[3..length(data)]
	at = match( "c ", data )
	if at = 0 then
	    -- badly formed color definition
	    return XPM_ERR_COLOR
	end if
    
	-- extract the color code
--	data = data[at+2..length(data)]
--	for j=length(data) to at+2 by -1 do
	at+=2
	for j=length(data) to at by -1 do
	    if data[j]!=' ' then
		data = data[at..j]
		exit
	    elsif j<=at then
		return XPM_ERR_COLOR
	    end if
	end for

	-- convert to an {r,g,b} code
	colorRGB[i] = convertToRGB( data, clearRGB )
	
	-- increment line
	line += 1
	
    end for

    -- check condition
    if errFlag then	    
	-- error in call to colorRGB
	return XPM_ERR_COLOR
    end if

    -- create data area
    bits = repeat( repeat( 0, bitsWide ), bitsTall )

    -- convert the text into indexes
    for i = 1 to bitsTall do

	-- get a line
	data = xpm[line]

	-- convert to lookup
	at = 0

	for j = 1 to length( data ) by codeWide do
	    
	    -- move 1 pixel at a time
	    at += 1		     
	    
	    -- find the position, subtract to make base zero
	    bits[i][at] = find( data[j..j+codeWide-1], colorCode ) - 1
	    
	end for

	-- read the next line
	line += 1
	
    end for

    return { colorRGB, bits }

end function


------------------------------------------------------------------------------
global function readXpm( sequence fName, sequence clearRGB )

    -- read a file into a sequence
    -- removes line feeds		
    
    integer handle, at
    sequence file
    object data
    
    file = {}
    
    -- open the file
    handle = open( fName, "r" )
    if handle = -1 then
	-- error opening file
	return XPM_ERR_FILE
    end if

    -- check header
    data = gets( handle )
    if not match( "/* XPM */", data ) then 
	return XPM_ERR_HEADER
    end if
    
    -- read to eof
    while 1 do	  
    
	-- read a line
	data = gets( handle )
	
	-- eof?
	if integer( data ) then
	    exit
	end if 

	-- remove line feed, if any
	if data[length(data)] = '\n' then
	    data = data[1..length(data)-1]
	end if

	-- blank line?
	if length( data ) = 0 then
	    -- ignore	    
	    
	-- not quoted?
--	elsif data[1] != '"' then
--	    -- ignore

	else
		    
	    -- first quote
	    at = find( '"', data )
for j=1 to at-1 do
    if not find(data[j]," \t") then
	at=0
	exit
    end if
end for
if at then
	    data = data[at+1..length(data)]
	
	    -- last quote
	    at = find( '"', data )
	    data = data[1..at-1]
			
	    -- add to file
	    file = append( file, data )
end if
	    
	end if
	
    end while
    
    -- close the file
    close( handle )
    
    -- reads an XPM from disk, returns bitmap
    return XpmToBmp( file, clearRGB )	       
    
end function

