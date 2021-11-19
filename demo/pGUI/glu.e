--
-- Copyright 1991-1993, Silicon Graphics, Inc.
-- All Rights Reserved.
--
-- This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
-- the contents of this file may not be disclosed to third parties, copied or
-- duplicated in any form, in whole or in part, without the prior written
-- permission of Silicon Graphics, Inc.
--
-- RESTRICTED RIGHTS LEGEND:
-- Use, duplication or disclosure by the Government is subject to restrictions
-- as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
-- and Computer Software clause at DFARS 252.227-7013, and/or in similar or
-- successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
-- rights reserved under the Copyright Laws of the United States.
--


-- Euphoria version by Mic, 000304
--
-- Glue-specific version, 000314
-- Updated 030126

--/*
include std/dll.e
include std/os.e
include std/machine.e
include std/convert.e
--*/
include opengl.e

constant GLUnurbsPtr = C_PTR,
         GLUquadricPtr = C_PTR,
         GLUtesselatorPtr = C_PTR

--***********************************************************

                -- Extensions
global constant GLU_EXT_object_space_tess = 1,
                GLU_EXT_nurbs_tessellator = 1,

                -- Boolean
                GLU_FALSE = 0,
                GLU_TRUE  = 1,

                -- Version
                GLU_VERSION_1_1 = 1,
                GLU_VERSION_1_2 = 1,

                -- StringName
                GLU_VERSION                     = 100800,
                GLU_EXTENSIONS                  = 100801,

                -- ErrorCode
                GLU_INVALID_ENUM                = 100900,
                GLU_INVALID_VALUE               = 100901,
                GLU_OUT_OF_MEMORY               = 100902,
                GLU_INCOMPATIBLE_GL_VERSION     = 100903,
                GLU_INVALID_OPERATION           = 100904,

                -- NurbsDisplay
--              GLU_FILL
                GLU_OUTLINE_POLYGON             = 100240,
                GLU_OUTLINE_PATCH               = 100241,

                -- NurbsCallback
                GLU_ERROR                       = 100103,

                -- NurbsError
                GLU_NURBS_ERROR1                = 100251,
                GLU_NURBS_ERROR2                = 100252,
                GLU_NURBS_ERROR3                = 100253,
                GLU_NURBS_ERROR4                = 100254,
                GLU_NURBS_ERROR5                = 100255,
                GLU_NURBS_ERROR6                = 100256,
                GLU_NURBS_ERROR7                = 100257,
                GLU_NURBS_ERROR8                = 100258,
                GLU_NURBS_ERROR9                = 100259,
                GLU_NURBS_ERROR10               = 100260,
                GLU_NURBS_ERROR11               = 100261,
                GLU_NURBS_ERROR12               = 100262,
                GLU_NURBS_ERROR13               = 100263,
                GLU_NURBS_ERROR14               = 100264,
                GLU_NURBS_ERROR15               = 100265,
                GLU_NURBS_ERROR16               = 100266,
                GLU_NURBS_ERROR17               = 100267,
                GLU_NURBS_ERROR18               = 100268,
                GLU_NURBS_ERROR19               = 100269,
                GLU_NURBS_ERROR20               = 100270,
                GLU_NURBS_ERROR21               = 100271,
                GLU_NURBS_ERROR22               = 100272,
                GLU_NURBS_ERROR23               = 100273,
                GLU_NURBS_ERROR24               = 100274,
                GLU_NURBS_ERROR25               = 100275,
                GLU_NURBS_ERROR26               = 100276,
                GLU_NURBS_ERROR27               = 100277,
                GLU_NURBS_ERROR28               = 100278,
                GLU_NURBS_ERROR29               = 100279,
                GLU_NURBS_ERROR30               = 100280,
                GLU_NURBS_ERROR31               = 100281,
                GLU_NURBS_ERROR32               = 100282,
                GLU_NURBS_ERROR33               = 100283,
                GLU_NURBS_ERROR34               = 100284,
                GLU_NURBS_ERROR35               = 100285,
                GLU_NURBS_ERROR36               = 100286,
                GLU_NURBS_ERROR37               = 100287,

                -- NurbsProperty
                GLU_AUTO_LOAD_MATRIX            = 100200,
                GLU_CULLING                     = 100201,
                GLU_SAMPLING_TOLERANCE          = 100203,
                GLU_DISPLAY_MODE                = 100204,
                GLU_PARAMETRIC_TOLERANCE        = 100202,
                GLU_SAMPLING_METHOD             = 100205,
                GLU_U_STEP                      = 100206,
                GLU_V_STEP                      = 100207,

                -- NurbsSampling
                GLU_OBJECT_PARAMETRIC_ERROR_EXT = 100208,
                GLU_OBJECT_PATH_LENGTH_EXT      = 100209,
                GLU_PATH_LENGTH                 = 100215,
                GLU_PARAMETRIC_ERROR            = 100216,
                GLU_DOMAIN_DISTANCE             = 100217,

                -- NurbsTrim
                GLU_MAP1_TRIM_2                 = 100210,
                GLU_MAP1_TRIM_3                 = 100211,

                -- QuadricCallback
--              GLU_ERROR

                -- QuadricNormal
                GLU_SMOOTH                      = 100000,
                GLU_FLAT                        = 100001,
                GLU_NONE                        = 100002,

                -- QuadricDrawStyle
                GLU_POINT                       = 100010,
                GLU_LINE                        = 100011,
                GLU_FILL                        = 100012,
                GLU_SILHOUETTE                  = 100013,
                
                -- QuadricOrientation
                GLU_OUTSIDE                     = 100020,
                GLU_INSIDE                      = 100021,

                -- TessCallback
                GLU_TESS_BEGIN                  = 100100,
                GLU_BEGIN                       = 100100,
                GLU_TESS_VERTEX                 = 100101,
                GLU_VERTEX                      = 100101,
                GLU_TESS_END                    = 100102,
                GLU_END                         = 100102,
                GLU_TESS_ERROR                  = 100103,
                GLU_TESS_EDGE_FLAG              = 100104,
                GLU_EDGE_FLAG                   = 100104,
                GLU_TESS_COMBINE                = 100105,
                GLU_TESS_BEGIN_DATA             = 100106,
                GLU_TESS_VERTEX_DATA            = 100107,
                GLU_TESS_END_DATA               = 100108,
                GLU_TESS_ERROR_DATA             = 100109,
                GLU_TESS_EDGE_FLAG_DATA         = 100110,
                GLU_TESS_COMBINE_DATA           = 100111,

                -- TessContour
                GLU_CW                          = 100120,
                GLU_CCW                         = 100121,
                GLU_INTERIOR                    = 100122,
                GLU_EXTERIOR                    = 100123,
                GLU_UNKNOWN                     = 100124,

                -- TessWinding
                GLU_TESS_WINDING_ODD            = 100130,
                GLU_TESS_WINDING_NONZERO        = 100131,
                GLU_TESS_WINDING_POSITIVE       = 100132,
                GLU_TESS_WINDING_NEGATIVE       = 100133,
                GLU_TESS_WINDING_ABS_GEQ_TWO    = 100134,

                -- TessProperty
                GLU_TESS_WINDING_RULE           = 100140,
                GLU_TESS_BOUNDARY_ONLY          = 100141,
                GLU_TESS_TOLERANCE              = 100142,

                -- TessError
                GLU_TESS_ERROR1                 = 100151,
                GLU_TESS_ERROR2                 = 100152,
                GLU_TESS_ERROR3                 = 100153,
                GLU_TESS_ERROR4                 = 100154,
                GLU_TESS_ERROR5                 = 100155,
                GLU_TESS_ERROR6                 = 100156,
                GLU_TESS_ERROR7                 = 100157,
                GLU_TESS_ERROR8                 = 100158,
                GLU_TESS_MISSING_BEGIN_POLYGON  = 100151,
                GLU_TESS_MISSING_BEGIN_CONTOUR  = 100152,
                GLU_TESS_MISSING_END_POLYGON    = 100153,
                GLU_TESS_MISSING_END_CONTOUR    = 100154,
                GLU_TESS_COORD_TOO_LARGE        = 100155,
                GLU_TESS_NEED_COMBINE_CALLBACK  = 100156,

                GLU_NURBS_MODE_EXT              = 100160,
                GLU_NURBS_TESSELLATOR_EXT       = 100161,
                GLU_NURBS_RENDERER_EXT          = 100162,
                GLU_NURBS_BEGIN_EXT             = 100164,
                GLU_NURBS_VERTEX_EXT            = 100165,
                GLU_NURBS_NORMAL_EXT            = 100166,
                GLU_NURBS_COLOR_EXT             = 100167,
                GLU_NURBS_TEX_COORD_EXT         = 100168,
                GLU_NURBS_END_EXT               = 100169,
                GLU_NURBS_BEGIN_DATA_EXT        = 100170,
                GLU_NURBS_VERTEX_DATA_EXT       = 100171,
                GLU_NURBS_NORMAL_DATA_EXT       = 100172,
                GLU_NURBS_COLOR_DATA_EXT        = 100173,
                GLU_NURBS_TEX_COORD_DATA_EXT    = 100174,
                GLU_NURBS_END_DATA_EXT          = 100175

--***********************************************************


string dll_so = iff(platform()=LINUX?GL_LIBPATH & "libGLU.so.1":"glu32.dll")
atom glu32 = open_dll(dll_so)
if glu32 = NULL then crash("Error! Can\'t find " & dll_so) end if

global constant
xgluBeginPolygon        = iup_c_proc(glu32,"gluBeginPolygon",{GLUtesselatorPtr}),
xgluBeginSurface        = iup_c_proc(glu32,"gluBeginSurface",{GLUnurbsPtr}),
xgluBeginTrim           = iup_c_proc(glu32,"gluBeginTrim",{GLUnurbsPtr}),
xgluBuild2DMipmaps      = iup_c_func(glu32,"gluBuild2DMipmaps",{C_INT,C_INT,C_INT,C_INT,C_INT,C_INT,C_PTR},C_INT),
xgluCylinder            = iup_c_proc(glu32,"gluCylinder",{GLUquadricPtr,C_DOUBLE,C_DOUBLE,C_DOUBLE,C_INT,C_INT}),
xgluDeleteNurbsRenderer = iup_c_proc(glu32,"gluDeleteNurbsRenderer",{GLUnurbsPtr}),
xgluDeleteQuadric       = iup_c_proc(glu32,"gluDeleteQuadric",{GLUquadricPtr}),
xgluDeleteTess          = iup_c_proc(glu32,"gluDeleteTess",{C_PTR}),
xgluDisk                = iup_c_proc(glu32,"gluDisk",{GLUquadricPtr,C_DOUBLE,C_DOUBLE,C_INT,C_INT}),
xgluEndCurve            = iup_c_proc(glu32,"gluEndCurve",{GLUnurbsPtr}),
xgluEndPolygon          = iup_c_proc(glu32,"gluEndPolygon",{GLUtesselatorPtr}),
xgluEndSurface          = iup_c_proc(glu32,"gluEndSurface",{GLUnurbsPtr}),
xgluEndTrim             = iup_c_proc(glu32,"gluEndTrim",{GLUnurbsPtr}),
xgluLookAt              = iup_c_proc(glu32,"gluLookAt",repeat(C_DOUBLE,9)),
xgluNewNurbsRenderer    = iup_c_func(glu32,"gluNewNurbsRenderer",{},GLUnurbsPtr),
xgluNewQuadric          = iup_c_func(glu32,"gluNewQuadric",{},GLUquadricPtr),
xgluNewTess             = iup_c_func(glu32,"gluNewTess",{},C_PTR),
xgluNurbsProperty       = iup_c_proc(glu32,"gluNurbsProperty",{GLUnurbsPtr,C_INT,C_FLOAT}),
xgluNurbsSurface        = iup_c_proc(glu32,"gluNurbsSurface",{GLUnurbsPtr,C_INT,C_PTR,C_INT,C_PTR,C_INT,C_INT,C_PTR,C_INT,C_INT,C_INT}),
xgluPerspective         = iup_c_proc(glu32,"gluPerspective",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
xgluProject             = iup_c_func(glu32,"gluProject",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR},C_INT),
xgluPwlCurve            = iup_c_proc(glu32,"gluPwlCurve",{GLUnurbsPtr,C_INT,C_PTR,C_INT,C_INT}),
xgluQuadricDrawStyle    = iup_c_proc(glu32,"gluQuadricDrawStyle",{GLUquadricPtr,C_INT}),
xgluQuadricNormals      = iup_c_proc(glu32,"gluQuadricNormals",{GLUquadricPtr, C_INT}),
xgluQuadricOrientation  = iup_c_proc(glu32,"gluQuadricOrientation",{GLUquadricPtr,C_INT}),
xgluQuadricTexture      = iup_c_proc(glu32,"gluQuadricTexture",{GLUquadricPtr, C_INT}),
xgluScaleImage          = iup_c_func(glu32,"gluScaleImage",{C_INT, C_INT, C_INT, C_INT, C_PTR, C_INT, C_INT, C_INT, C_PTR},C_INT),
xgluSphere              = iup_c_proc(glu32,"gluSphere",{GLUquadricPtr,C_DOUBLE,C_INT,C_INT}),
xgluTessBeginContour    = iup_c_proc(glu32,"gluTessBeginContour",{C_PTR}),
xgluTessBeginPolygon    = iup_c_proc(glu32,"gluTessBeginPolygon",{C_PTR,C_PTR}),
xgluTessCallback        = iup_c_proc(glu32,"gluTessCallback",{C_PTR,C_INT,C_PTR}),
xgluTessEndContour      = iup_c_proc(glu32,"gluTessEndContour",{C_PTR}),
xgluTessEndPolygon      = iup_c_proc(glu32,"gluTessEndPolygon",{C_PTR}),
xgluTessNormal          = iup_c_proc(glu32,"gluTessNormal",{C_PTR,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
xgluTessProperty        = iup_c_proc(glu32,"gluTessProperty",{C_PTR,C_INT,C_DOUBLE}),
xgluTessVertex          = iup_c_proc(glu32,"gluTessVertex",{C_PTR,C_PTR,C_PTR}),
xgluUnProject           = iup_c_func(glu32,"gluUnProject",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR},C_INT)


constant glu_buffer = allocate(512)


procedure glu_pokef32(atom dest,sequence data)
    for i=1 to length(data) do
        object datai = data[i]
        if sequence(datai) then
            for j=1 to length(datai) do
                poke(dest,atom_to_float32(datai[j]))
                dest += 4
            end for
        else
            poke(dest,atom_to_float32(datai))
            dest += 4
        end if
    end for
end procedure


global procedure gluBeginSurface(atom nurb)
    c_proc(xgluBeginSurface,{nurb})
end procedure

global procedure gluBeginTrim(atom nurb)
    c_proc(xgluBeginTrim,{nurb})
end procedure

global function gluBuild2DMipmaps(integer target, component, width, height, fmt, typ, atom data)
    return c_func(xgluBuild2DMipmaps,{target,component,width,height,fmt,typ,data})
end function

global procedure gluDeleteNurbsRenderer(atom nurb)
    c_proc(xgluDeleteNurbsRenderer,{nurb})
end procedure

global procedure gluDeleteQuadric(atom quadric)
    c_proc(xgluDeleteQuadric,{quadric})
end procedure

global procedure gluEndSurface(atom nurb)
    c_proc(xgluEndSurface,{nurb})
end procedure

global procedure gluEndTrim(atom nurb)
    c_proc(xgluEndTrim,{nurb})
end procedure

global procedure gluLookAt(sequence eye, center, up)
    c_proc(xgluLookAt,eye&center&up)
end procedure

global function gluNewNurbsRenderer()
    return c_func(xgluNewNurbsRenderer,{})
end function

global function gluNewQuadric()
    return c_func(xgluNewQuadric,{})
end function

global procedure gluNurbsProperty(atom nurb, integer property, atom val)
    c_proc(xgluNurbsProperty,{nurb,property,val})
end procedure

global procedure gluNurbsSurface(atom nurb, integer sKnotCount,sequence sKnots, integer tKnotCount, sequence tKnots,
                                 integer sStride, tStride, sequence control, integer sOrder, tOrder, typ)
    --SUG: simplify? check limits?
    if sKnotCount!=length(sKnots) then ?9/0 end if
    if tKnotCount!=length(tKnots) then ?9/0 end if
    glu_pokef32(glu_buffer,sKnots)
    glu_pokef32(glu_buffer+128,tKnots)
    glu_pokef32(glu_buffer+256,control)
    c_proc(xgluNurbsSurface,{nurb,sKnotCount,glu_buffer,tKnotCount,glu_buffer+128,sStride,tStride,glu_buffer+256,sOrder,tOrder,typ})
end procedure

global procedure gluPerspective(atom fovy, aspect, zNear, zFar)
    c_proc(xgluPerspective,{fovy,aspect,zNear,zFar})
end procedure

global procedure gluProject(atom objx, objy, objz, pModelMatrix, pProjMatrix, pViewport, winx, winy, winz)
    integer res = c_func(xgluProject,{objx,objy,objz,pModelMatrix,pProjMatrix,pViewport,winx,winy,winz})
    if res!=GL_TRUE then ?9/0 end if
end procedure

global procedure gluPwlCurve(atom nurb, integer count, sequence data, integer stride, integer typ)
    if count!=length(data) then ?9/0 end if
    glu_pokef32(glu_buffer,data)
    c_proc(xgluPwlCurve,{nurb,count,glu_buffer,stride,typ})
end procedure

global procedure gluQuadricDrawStyle(atom quadric, integer draw)
    c_proc(xgluQuadricDrawStyle,{quadric,draw})
end procedure

global procedure gluQuadricNormals(atom quadric, integer normal)
    c_proc(xgluQuadricNormals,{quadric,normal})
end procedure

global procedure gluQuadricOrientation(atom quadric, integer orientation)
    c_proc(xgluQuadricOrientation,{quadric,orientation})
end procedure

global procedure gluQuadricTexture(atom quadric, integer texture)
    c_proc(xgluQuadricTexture,{quadric,texture})
end procedure

global function gluScaleImage(integer fmt, wIn, hIn, typeIn, atom dataIn,
                              integer wOut, hOut, typeOut, atom dataOut)
    return c_func(xgluScaleImage,{fmt,wIn,hIn,typeIn,dataIn,wOut,hOut,typeOut,dataOut})
end function

global procedure gluSphere(atom quadric, radius,integer slices, stacks)
    c_proc(xgluSphere,{quadric,radius,slices,stacks})
end procedure

global procedure gluTessCallback(atom tess, integer which, atom cb)
    c_proc(xgluTessCallback,{tess,which,cb})
end procedure

global procedure gluUnProject(atom x, y, z, pModelMatrix, pProjMatrix, pViewport, pX, pY, pZ)
    integer res = c_func(xgluUnProject,{x, y, z, pModelMatrix, pProjMatrix, pViewport, pX, pY, pZ})
    if res!=GL_TRUE then ?9/0 end if
end procedure

