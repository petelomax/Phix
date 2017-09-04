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

constant
    GLUnurbsPtr = C_PTR,
    GLUquadricPtr   = C_PTR,
    GLUtesselatorPtr= C_PTR

--***********************************************************

-- Extensions
global constant GLU_EXT_object_space_tess = 1
global constant GLU_EXT_nurbs_tessellator = 1

-- Boolean
global constant GLU_FALSE = 0
global constant GLU_TRUE = 1

-- Version
global constant GLU_VERSION_1_1 = 1
global constant GLU_VERSION_1_2 = 1

-- StringName
global constant GLU_VERSION = 100800
global constant GLU_EXTENSIONS = 100801

-- ErrorCode
global constant GLU_INVALID_ENUM = 100900
global constant GLU_INVALID_VALUE = 100901
global constant GLU_OUT_OF_MEMORY = 100902
global constant GLU_INCOMPATIBLE_GL_VERSION = 100903
global constant GLU_INVALID_OPERATION = 100904

-- NurbsDisplay
--      GLU_FILL
global constant GLU_OUTLINE_POLYGON = 100240
global constant GLU_OUTLINE_PATCH = 100241

-- NurbsCallback
global constant GLU_ERROR = 100103

-- NurbsError
global constant GLU_NURBS_ERROR1 = 100251
global constant GLU_NURBS_ERROR2 = 100252
global constant GLU_NURBS_ERROR3 = 100253
global constant GLU_NURBS_ERROR4 = 100254
global constant GLU_NURBS_ERROR5 = 100255
global constant GLU_NURBS_ERROR6 = 100256
global constant GLU_NURBS_ERROR7 = 100257
global constant GLU_NURBS_ERROR8 = 100258
global constant GLU_NURBS_ERROR9 = 100259
global constant GLU_NURBS_ERROR10 = 100260
global constant GLU_NURBS_ERROR11 = 100261
global constant GLU_NURBS_ERROR12 = 100262
global constant GLU_NURBS_ERROR13 = 100263
global constant GLU_NURBS_ERROR14 = 100264
global constant GLU_NURBS_ERROR15 = 100265
global constant GLU_NURBS_ERROR16 = 100266
global constant GLU_NURBS_ERROR17 = 100267
global constant GLU_NURBS_ERROR18 = 100268
global constant GLU_NURBS_ERROR19 = 100269
global constant GLU_NURBS_ERROR20 = 100270
global constant GLU_NURBS_ERROR21 = 100271
global constant GLU_NURBS_ERROR22 = 100272
global constant GLU_NURBS_ERROR23 = 100273
global constant GLU_NURBS_ERROR24 = 100274
global constant GLU_NURBS_ERROR25 = 100275
global constant GLU_NURBS_ERROR26 = 100276
global constant GLU_NURBS_ERROR27 = 100277
global constant GLU_NURBS_ERROR28 = 100278
global constant GLU_NURBS_ERROR29 = 100279
global constant GLU_NURBS_ERROR30 = 100280
global constant GLU_NURBS_ERROR31 = 100281
global constant GLU_NURBS_ERROR32 = 100282
global constant GLU_NURBS_ERROR33 = 100283
global constant GLU_NURBS_ERROR34 = 100284
global constant GLU_NURBS_ERROR35 = 100285
global constant GLU_NURBS_ERROR36 = 100286
global constant GLU_NURBS_ERROR37 = 100287

-- NurbsProperty
global constant GLU_AUTO_LOAD_MATRIX = 100200
global constant GLU_CULLING = 100201
global constant GLU_SAMPLING_TOLERANCE = 100203
global constant GLU_DISPLAY_MODE = 100204
global constant GLU_PARAMETRIC_TOLERANCE = 100202
global constant GLU_SAMPLING_METHOD = 100205
global constant GLU_U_STEP = 100206
global constant GLU_V_STEP = 100207

-- NurbsSampling
global constant GLU_OBJECT_PARAMETRIC_ERROR_EXT = 100208
global constant GLU_OBJECT_PATH_LENGTH_EXT = 100209
global constant GLU_PATH_LENGTH = 100215
global constant GLU_PARAMETRIC_ERROR = 100216
global constant GLU_DOMAIN_DISTANCE = 100217

-- NurbsTrim
global constant GLU_MAP1_TRIM_2 = 100210
global constant GLU_MAP1_TRIM_3 = 100211

-- QuadricDrawStyle
global constant GLU_POINT = 100010
global constant GLU_LINE = 100011
global constant GLU_FILL = 100012
global constant GLU_SILHOUETTE = 100013

-- QuadricCallback
--      GLU_ERROR

-- QuadricNormal
global constant GLU_SMOOTH = 100000
global constant GLU_FLAT = 100001
global constant GLU_NONE = 100002

-- QuadricOrientation
global constant GLU_OUTSIDE = 100020
global constant GLU_INSIDE = 100021

-- TessCallback
global constant GLU_TESS_BEGIN = 100100
global constant GLU_BEGIN = 100100
global constant GLU_TESS_VERTEX = 100101
global constant GLU_VERTEX = 100101
global constant GLU_TESS_END = 100102
global constant GLU_END = 100102
global constant GLU_TESS_ERROR = 100103
global constant GLU_TESS_EDGE_FLAG = 100104
global constant GLU_EDGE_FLAG = 100104
global constant GLU_TESS_COMBINE = 100105
global constant GLU_TESS_BEGIN_DATA = 100106
global constant GLU_TESS_VERTEX_DATA = 100107
global constant GLU_TESS_END_DATA = 100108
global constant GLU_TESS_ERROR_DATA = 100109
global constant GLU_TESS_EDGE_FLAG_DATA = 100110
global constant GLU_TESS_COMBINE_DATA = 100111
global constant GLU_NURBS_MODE_EXT = 100160
global constant GLU_NURBS_TESSELLATOR_EXT = 100161
global constant GLU_NURBS_RENDERER_EXT = 100162
global constant GLU_NURBS_BEGIN_EXT = 100164
global constant GLU_NURBS_VERTEX_EXT = 100165
global constant GLU_NURBS_NORMAL_EXT = 100166
global constant GLU_NURBS_COLOR_EXT = 100167
global constant GLU_NURBS_TEX_COORD_EXT = 100168
global constant GLU_NURBS_END_EXT = 100169
global constant GLU_NURBS_BEGIN_DATA_EXT = 100170
global constant GLU_NURBS_VERTEX_DATA_EXT = 100171
global constant GLU_NURBS_NORMAL_DATA_EXT = 100172
global constant GLU_NURBS_COLOR_DATA_EXT = 100173
global constant GLU_NURBS_TEX_COORD_DATA_EXT = 100174
global constant GLU_NURBS_END_DATA_EXT = 100175

-- TessContour
global constant GLU_CW = 100120
global constant GLU_CCW = 100121
global constant GLU_INTERIOR = 100122
global constant GLU_EXTERIOR = 100123
global constant GLU_UNKNOWN = 100124

-- TessProperty
global constant GLU_TESS_WINDING_RULE = 100140
global constant GLU_TESS_BOUNDARY_ONLY = 100141
global constant GLU_TESS_TOLERANCE = 100142

-- TessError
global constant GLU_TESS_ERROR1 = 100151
global constant GLU_TESS_ERROR2 = 100152
global constant GLU_TESS_ERROR3 = 100153
global constant GLU_TESS_ERROR4 = 100154
global constant GLU_TESS_ERROR5 = 100155
global constant GLU_TESS_ERROR6 = 100156
global constant GLU_TESS_ERROR7 = 100157
global constant GLU_TESS_ERROR8 = 100158
global constant GLU_TESS_MISSING_BEGIN_POLYGON = 100151
global constant GLU_TESS_MISSING_BEGIN_CONTOUR = 100152
global constant GLU_TESS_MISSING_END_POLYGON = 100153
global constant GLU_TESS_MISSING_END_CONTOUR = 100154
global constant GLU_TESS_COORD_TOO_LARGE = 100155
global constant GLU_TESS_NEED_COMBINE_CALLBACK = 100156

-- TessWinding
global constant GLU_TESS_WINDING_ODD = 100130
global constant GLU_TESS_WINDING_NONZERO = 100131
global constant GLU_TESS_WINDING_POSITIVE = 100132
global constant GLU_TESS_WINDING_NEGATIVE = 100133
global constant GLU_TESS_WINDING_ABS_GEQ_TWO = 100134

--***********************************************************


atom glu32

if platform()=LINUX then
    glu32 = open_dll(GL_LIBPATH & "libGLU.so.1")
else
    glu32 = open_dll("glu32.dll")
end if

if glu32 = NULL then
    if platform()=LINUX then
        puts(1,"Error! Can\'t find " & GL_LIBPATH & "libGLU.so.1")
    else
        puts(1,"Error! Can\'t find glu32.dll")
    end if
end if

global constant
GluBeginPolygon         = validate_proc(glu32,"gluBeginPolygon",{GLUtesselatorPtr}),
GluBeginSurface         = validate_proc(glu32,"gluBeginSurface",{GLUnurbsPtr}),
GluBeginTrim            = validate_proc(glu32,"gluBeginTrim",{GLUnurbsPtr}),
GluBuild2DMipmaps       = validate_func(glu32,"gluBuild2DMipmaps",{C_INT,C_INT,C_INT,C_INT,C_INT,C_INT,C_PTR},C_INT),
GluCylinder             = validate_proc(glu32,"gluCylinder",{GLUquadricPtr,C_DOUBLE,C_DOUBLE,C_DOUBLE,C_INT,C_INT}),
GluDeleteNurbsRenderer  = validate_proc(glu32,"gluDeleteNurbsRenderer",{GLUnurbsPtr}),
GluDeleteQuadric        = validate_proc(glu32,"gluDeleteQuadric",{GLUquadricPtr}),
GluDeleteTess           = validate_proc(glu32,"gluDeleteTess",{C_PTR}),
GluDisk                 = validate_proc(glu32,"gluDisk",{GLUquadricPtr,C_DOUBLE,C_DOUBLE,C_INT,C_INT}),
GluEndCurve             = validate_proc(glu32,"gluEndCurve",{GLUnurbsPtr}),
GluEndPolygon           = validate_proc(glu32,"gluEndPolygon",{GLUtesselatorPtr}),
GluEndSurface           = validate_proc(glu32,"gluEndSurface",{GLUnurbsPtr}),
GluEndTrim              = validate_proc(glu32,"gluEndTrim",{GLUnurbsPtr}),
GluLookAt               = validate_proc(glu32,"gluLookAt",repeat(C_DOUBLE,9)),
GluNewNurbsRenderer     = validate_func(glu32,"gluNewNurbsRenderer",{},GLUnurbsPtr),
GluNewQuadric           = validate_func(glu32,"gluNewQuadric",{},GLUquadricPtr),
GluNewTess              = validate_func(glu32,"gluNewTess",{},C_PTR),
GluNurbsProperty        = validate_proc(glu32,"gluNurbsProperty",{GLUnurbsPtr,C_INT,C_FLOAT}),
GluNurbsSurface         = validate_proc(glu32,"gluNurbsSurface",{GLUnurbsPtr,C_INT,C_PTR,C_INT,C_PTR,C_INT,C_INT,C_PTR,C_INT,C_INT,C_INT}),
GluPerspective          = validate_proc(glu32,"gluPerspective",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
GluProject              = validate_func(glu32,"gluProject",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR},C_INT),
GluPwlCurve             = validate_proc(glu32,"gluPwlCurve",{GLUnurbsPtr,C_INT,C_PTR,C_INT,C_INT}),
GluQuadricDrawStyle     = validate_proc(glu32,"gluQuadricDrawStyle",{GLUquadricPtr,C_INT}),
GluQuadricNormals       = validate_proc(glu32,"gluQuadricNormals",{GLUquadricPtr, C_INT}),
GluQuadricOrientation   = validate_proc(glu32,"gluQuadricOrientation",{GLUquadricPtr,C_INT}),
GluQuadricTexture       = validate_proc(glu32,"gluQuadricTexture",{GLUquadricPtr, C_INT}),
GluScaleImage           = validate_func(glu32,"gluScaleImage",{C_INT, C_INT, C_INT, C_INT, C_PTR, C_INT, C_INT, C_INT, C_PTR},C_INT),
GluSphere               = validate_proc(glu32,"gluSphere",{GLUquadricPtr,C_DOUBLE,C_INT,C_INT}),
GluTessBeginContour     = validate_proc(glu32,"gluTessBeginContour",{C_PTR}),
GluTessBeginPolygon     = validate_proc(glu32,"gluTessBeginPolygon",{C_PTR,C_PTR}),
GluTessCallback         = validate_proc(glu32,"gluTessCallback",{C_PTR,C_INT,C_PTR}),
GluTessEndContour       = validate_proc(glu32,"gluTessEndContour",{C_PTR}),
GluTessEndPolygon       = validate_proc(glu32,"gluTessEndPolygon",{C_PTR}),
GluTessNormal           = validate_proc(glu32,"gluTessNormal",{C_PTR,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
GluTessProperty         = validate_proc(glu32,"gluTessProperty",{C_PTR,C_INT,C_DOUBLE}),
GluTessVertex           = validate_proc(glu32,"gluTessVertex",{C_PTR,C_PTR,C_PTR}),
GluUnProject            = validate_func(glu32,"gluUnProject",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR},C_INT)


constant glu_buffer = allocate(512)


procedure glu_pokef32(atom dest,sequence data)
    sequence datai

    for i = 1 to length(data) do
        if sequence(data[i]) then
            datai = data[i]
            for j=1 to length(datai) do
                poke(dest,atom_to_float32(datai[j]))
                dest += 4
            end for
        else
            poke(dest,atom_to_float32(data[i]))
            dest += 4
        end if
    end for
end procedure


global procedure gluBeginSurface(atom nurb)
    c_proc(GluBeginSurface,{nurb})
end procedure

global procedure gluBeginTrim(atom nurb)
    c_proc(GluBeginTrim,{nurb})
end procedure

global function gluBuild2DMipmaps(integer target, integer component, integer width, integer height, integer fmt, integer typ, atom data)
    return c_func(GluBuild2DMipmaps,{target,component,width,height,fmt,typ,data})
end function

global procedure gluDeleteNurbsRenderer(atom nurb)
    c_proc(GluDeleteNurbsRenderer,{nurb})
end procedure

global procedure gluDeleteQuadric(atom quadric)
    c_proc(GluDeleteQuadric,{quadric})
end procedure

global procedure gluEndSurface(atom nurb)
    c_proc(GluEndSurface,{nurb})
end procedure

global procedure gluEndTrim(atom nurb)
    c_proc(GluEndTrim,{nurb})
end procedure

global procedure gluLookAt(sequence eye,sequence center,sequence up)
    c_proc(GluLookAt,eye&center&up)
end procedure

global function gluNewNurbsRenderer()
    return c_func(GluNewNurbsRenderer,{})
end function

global function gluNewQuadric()
    return c_func(GluNewQuadric,{})
end function

global procedure gluNurbsProperty(atom nurb, integer property, atom val)
    c_proc(GluNurbsProperty,{nurb,property,val})
end procedure

global procedure gluNurbsSurface(atom nurb,integer sKnotCount,sequence sKnots,integer tKnotCount,sequence tKnots,
                  integer sStride, integer tStride, sequence control, integer sOrder, integer tOrder,
                  integer typ)
    glu_pokef32(glu_buffer,sKnots)
    glu_pokef32(glu_buffer+128,tKnots)
    glu_pokef32(glu_buffer+256,control)
    c_proc(GluNurbsSurface,{nurb,sKnotCount,glu_buffer,tKnotCount,glu_buffer+128,sStride,tStride,glu_buffer+256,sOrder,tOrder,typ})
end procedure

global procedure gluPerspective(atom fovy, atom aspect, atom zNear, atom zFar)
    c_proc(GluPerspective,{fovy,aspect,zNear,zFar})
end procedure

global procedure gluProject(atom objx,atom objy,atom objz,atom pModelMatrix,atom pProjMatrix,atom pViewport,atom winx,atom winy,atom winz)
integer res
    res = c_func(GluProject,{objx,objy,objz,pModelMatrix,pProjMatrix,pViewport,winx,winy,winz})
    if res!=GL_TRUE then ?9/0 end if
end procedure

global procedure gluPwlCurve(atom nurb,integer count,sequence data,integer stride,integer typ)
    glu_pokef32(glu_buffer,data)
    c_proc(GluPwlCurve,{nurb,count,glu_buffer,stride,typ})
end procedure

global procedure gluQuadricDrawStyle(atom quadric,integer draw)
    c_proc(GluQuadricDrawStyle,{quadric,draw})
end procedure

global procedure gluQuadricNormals(atom quadric,integer normal)
    c_proc(GluQuadricNormals,{quadric,normal})
end procedure

global procedure gluQuadricOrientation(atom quadric,integer orien)
    c_proc(GluQuadricOrientation,{quadric,orien})
end procedure

global procedure gluQuadricTexture(atom quadric,integer texture)
    c_proc(GluQuadricTexture,{quadric,texture})
end procedure

global function gluScaleImage(integer format,integer wIn,integer hIn,integer typeIn,atom dataIn,
                   integer wOut,integer hOut,integer typeOut,atom dataOut)
    return c_func(GluScaleImage,{format,wIn,hIn,typeIn,dataIn,wOut,hOut,typeOut,dataOut})
end function

global procedure gluSphere(atom quadric,atom radius,integer slices,integer stacks)
    c_proc(GluSphere,{quadric,radius,slices,stacks})
end procedure

global procedure gluTessCallback(atom tess,integer which,atom cb)
    c_proc(GluTessCallback,{tess,which,cb})
end procedure

global procedure gluUnProject(atom x, atom y, atom z, atom pModelMatrix, atom pProjMatrix, atom pViewport, atom pX, atom pY, atom pZ)
integer res
    res = c_func(GluUnProject,{x, y, z, pModelMatrix, pProjMatrix, pViewport, pX, pY, pZ})
    if res!=GL_TRUE then ?9/0 end if
end procedure


