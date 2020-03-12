--
-- Copyright 1992-1997 Silicon Graphics, Inc.
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
-- Updated version, 000704
-- Updated version, 030126


--/*
include std/os.e
include std/machine.e
include std/dll.e
include std/get.e
include std/convert.e
--*/
--include validate.e

include pGUI.e

-- glBegin mode
global constant GL_POINTS         = 0,  -- Treats each vertex as a single point. 
                                        -- Vertex n defines point n. N points are drawn.
                GL_LINES          = 1,  -- Treats each pair of vertices as an independent line segment. 
                                        -- Vertices 2n - 1 and 2n define line n. N/2 lines are drawn.
                GL_LINE_LOOP      = 2,  -- Draws a connected group of line segments from the first vertex 
                                        -- to the last, then back to the first. 
                                        -- Vertices n and n + 1 define line n. 
                                        -- The last line, however, is defined by vertices N and 1. 
                                        -- N lines are drawn.
                GL_LINE_STRIP     = 3,  -- Draws a connected group of line segments from the first vertex 
                                        -- to the last. 
                                        -- Vertices n and n+1 define line n. N - 1 lines are drawn.
                GL_TRIANGLES      = 4,  -- Treats each triplet of vertices as an independent triangle. 
                                        -- Vertices 3n - 2, 3n - 1, and 3n define triangle n. 
                                        -- N/3 triangles are drawn.
                GL_TRIANGLE_STRIP = 5,  -- Draws a connected group of triangles. 
                                        -- One triangle is defined for each vertex presented after the 
                                        -- first two vertices. 
                                        -- For odd n, vertices n, n + 1, and n + 2 define triangle n. 
                                        -- For even n, vertices n + 1, n, and n + 2 define triangle n. 
                                        -- N - 2 triangles are drawn.
                GL_TRIANGLE_FAN   = 6,  -- Draws a connected group of triangles. 
                                        -- One triangle is defined for each vertex presented after the 
                                        -- first two vertices. 
                                        -- Vertices 1, n + 1, n + 2 define triangle n. 
                                        -- N - 2 triangles are drawn.
                GL_QUADS          = 7,  -- Treats each group of four vertices as an independent quadrilateral. 
                                        -- Vertices 4n - 3, 4n - 2, 4n - 1, and 4n define quadrilateral n. 
                                        -- N/4 quadrilaterals are drawn.
                GL_QUAD_STRIP     = 8,  -- Draws a connected group of quadrilaterals. 
                                        -- One quadrilateral is defined for each pair of vertices presented 
                                        -- after the first pair. 
                                        -- Vertices 2n - 1, 2n, 2n + 2, and 2n + 1 define quadrilateral n. 
                                        -- N/2 - 1 quadrilaterals are drawn. 
                                        -- Note that the order in which vertices are used to construct a 
                                        -- quadrilateral from strip data is different from independent data.
                GL_POLYGON        = 9   -- Draws a single, convex polygon. 
                                        -- Vertices 1 through N define this polygon.

-- ErrorCode
-- If non-zero, the offending function is ignored, having no side effect other than to set the error flag.
global constant GL_NO_ERROR             = 0,        -- No error has been recorded.
                GL_INVALID_ENUM         = #0500,    -- Unacceptable value specified for an enumerated argument.
                GL_INVALID_VALUE        = #0501,    -- A numeric argument is out of range. 
                GL_INVALID_OPERATION    = #0502,    -- Specified operation not allowed in the current state.
                GL_STACK_OVERFLOW       = #0503,    -- This function would cause a stack overflow.
                GL_STACK_UNDERFLOW      = #0504,    -- This function would cause a stack underflow.
                GL_OUT_OF_MEMORY        = #0505     -- There is not enough memory left to execute the function.
                                                    -- The state of OpenGL is undefined, except for the state 
                                                    --  of the error flags, after this error is recorded.
--              GL_TABLE_TOO_LARGE_EXT

-- StringName
global constant GL_VENDOR     = #1F00,  -- Returns the company responsible for this OpenGL implementation. 
                                        -- This name does not change from release to release.
                GL_RENDERER   = #1F01,  -- Returns the name of the renderer, typically specific to a 
                                        -- particular configuration of a hardware platform. 
                                        -- It does not change from release to release.
                GL_VERSION    = #1F02,  -- Returns a version or release number.
                GL_EXTENSIONS = #1F03   -- Returns a space-separated list of supported extensions to OpenGL.

-- ShadingModel
global constant GL_FLAT   = #1D00,
                GL_SMOOTH = #1D01

-- AttribMask
global constant GL_CURRENT_BIT          = #00000001,
                GL_POINT_BIT            = #00000002,
                GL_LINE_BIT             = #00000004,
                GL_POLYGON_BIT          = #00000008,
                GL_POLYGON_STIPPLE_BIT  = #00000010,
                GL_PIXEL_MODE_BIT       = #00000020,
                GL_LIGHTING_BIT         = #00000040,
                GL_FOG_BIT              = #00000080,
                GL_DEPTH_BUFFER_BIT     = #00000100, -- The depth buffer.
                GL_ACCUM_BUFFER_BIT     = #00000200, -- The accumulation buffer.
                GL_STENCIL_BUFFER_BIT   = #00000400, -- The stencil buffer.
                GL_VIEWPORT_BIT         = #00000800,
                GL_TRANSFORM_BIT        = #00001000,
                GL_ENABLE_BIT           = #00002000,
                GL_COLOR_BUFFER_BIT     = #00004000, -- The buffers currently enabled for color writing.
                GL_HINT_BIT             = #00008000,
                GL_EVAL_BIT             = #00010000,
                GL_LIST_BIT             = #00020000,
                GL_TEXTURE_BIT          = #00040000,
                GL_SCISSOR_BIT          = #00080000,
                GL_ALL_ATTRIB_BITS      = #000FFFFF

-- ClearBufferMask
--      GL_COLOR_BUFFER_BIT
--      GL_ACCUM_BUFFER_BIT
--      GL_STENCIL_BUFFER_BIT
--      GL_DEPTH_BUFFER_BIT

-- MatrixMode
global constant GL_MODELVIEW  = #1700,  -- Applies subsequent matrix operations to 
                                        -- the modelview matrix stack.
                GL_PROJECTION = #1701,  -- Applies subsequent matrix operations to 
                                        -- the projection matrix stack.
                GL_TEXTURE    = #1702   -- Applies subsequent matrix operations to 
                                        -- the texture matrix stack.

-- LightName
global constant GL_LIGHT0 = #4000,
                GL_LIGHT1 = #4001,
                GL_LIGHT2 = #4002,
                GL_LIGHT3 = #4003,
                GL_LIGHT4 = #4004,
                GL_LIGHT5 = #4005,
                GL_LIGHT6 = #4006,
                GL_LIGHT7 = #4007

-- LightParameter
global constant GL_AMBIENT               = #1200,
                GL_DIFFUSE               = #1201,
                GL_SPECULAR              = #1202,
                GL_POSITION              = #1203,
                GL_SPOT_DIRECTION        = #1204,
                GL_SPOT_EXPONENT         = #1205,
                GL_SPOT_CUTOFF           = #1206,
                GL_CONSTANT_ATTENUATION  = #1207,
                GL_LINEAR_ATTENUATION    = #1208,
                GL_QUADRATIC_ATTENUATION = #1209


-- EnableCap (subset of GetPName)
--      GL_FOG
--      GL_LIGHTING
--      GL_TEXTURE_1D
--      GL_TEXTURE_2D
--      GL_LINE_STIPPLE
--      GL_POLYGON_STIPPLE
--      GL_CULL_FACE
--      GL_ALPHA_TEST
--      GL_BLEND
--      GL_INDEX_LOGIC_OP
--      GL_COLOR_LOGIC_OP
--      GL_DITHER
--      GL_STENCIL_TEST
--      GL_DEPTH_TEST
--      GL_CLIP_PLANE0
--      GL_CLIP_PLANE1
--      GL_CLIP_PLANE2
--      GL_CLIP_PLANE3
--      GL_CLIP_PLANE4
--      GL_CLIP_PLANE5
--      GL_TEXTURE_GEN_S
--      GL_TEXTURE_GEN_T
--      GL_TEXTURE_GEN_R
--      GL_TEXTURE_GEN_Q
--      GL_MAP1_VERTEX_3
--      GL_MAP1_VERTEX_4
--      GL_MAP1_COLOR_4
--      GL_MAP1_INDEX
--      GL_MAP1_NORMAL
--      GL_MAP1_TEXTURE_COORD_1
--      GL_MAP1_TEXTURE_COORD_2
--      GL_MAP1_TEXTURE_COORD_3
--      GL_MAP1_TEXTURE_COORD_4
--      GL_MAP2_VERTEX_3
--      GL_MAP2_VERTEX_4
--      GL_MAP2_COLOR_4
--      GL_MAP2_INDEX
--      GL_MAP2_NORMAL
--      GL_MAP2_TEXTURE_COORD_1
--      GL_MAP2_TEXTURE_COORD_2
--      GL_MAP2_TEXTURE_COORD_3
--      GL_MAP2_TEXTURE_COORD_4
--      GL_POINT_SMOOTH
--      GL_LINE_SMOOTH
--      GL_POLYGON_SMOOTH
--      GL_SCISSOR_TEST
--      GL_COLOR_MATERIAL
--      GL_NORMALIZE
--      GL_AUTO_NORMAL
--      GL_POLYGON_OFFSET_POINT
--      GL_POLYGON_OFFSET_LINE
--      GL_POLYGON_OFFSET_FILL
--      GL_VERTEX_ARRAY
--      GL_NORMAL_ARRAY
--      GL_COLOR_ARRAY
--      GL_INDEX_ARRAY
--      GL_TEXTURE_COORD_ARRAY
--      GL_EDGE_FLAG_ARRAY
--      GL_CULL_VERTEX_SGI
--      GL_INDEX_MATERIAL_SGI
--      GL_INDEX_TEST_SGI


-- GetPName
global constant GL_CURRENT_COLOR                    = #0B00,
                GL_CURRENT_INDEX                    = #0B01,
                GL_CURRENT_NORMAL                   = #0B02,
                GL_CURRENT_TEXTURE_COORDS           = #0B03,
                GL_CURRENT_RASTER_COLOR             = #0B04,
                GL_CURRENT_RASTER_INDEX             = #0B05,
                GL_CURRENT_RASTER_TEXTURE_COORDS    = #0B06,
                GL_CURRENT_RASTER_POSITION          = #0B07,
                GL_CURRENT_RASTER_POSITION_VALID    = #0B08,
                GL_CURRENT_RASTER_DISTANCE          = #0B09,
                GL_POINT_SMOOTH                     = #0B10,
                GL_POINT_SIZE                       = #0B11,
                GL_POINT_SIZE_RANGE                 = #0B12,
                GL_POINT_SIZE_GRANULARITY           = #0B13,
                GL_LINE_SMOOTH                      = #0B20,
                GL_LINE_WIDTH                       = #0B21,
                GL_LINE_WIDTH_RANGE                 = #0B22,
                GL_LINE_WIDTH_GRANULARITY           = #0B23,
                GL_LINE_STIPPLE                     = #0B24,
                GL_LINE_STIPPLE_PATTERN             = #0B25,
                GL_LINE_STIPPLE_REPEAT              = #0B26,
                GL_LIST_MODE                        = #0B30,
                GL_MAX_LIST_NESTING                 = #0B31,
                GL_LIST_BASE                        = #0B32,
                GL_LIST_INDEX                       = #0B33,
                GL_POLYGON_MODE                     = #0B40,
                GL_POLYGON_SMOOTH                   = #0B41,
                GL_POLYGON_STIPPLE                  = #0B42,
                GL_EDGE_FLAG                        = #0B43,
                GL_CULL_FACE                        = #0B44,
                GL_CULL_FACE_MODE                   = #0B45,
                GL_FRONT_FACE                       = #0B46,
                GL_LIGHTING                         = #0B50,
                GL_LIGHT_MODEL_LOCAL_VIEWER         = #0B51,
                GL_LIGHT_MODEL_TWO_SIDE             = #0B52,
                GL_LIGHT_MODEL_AMBIENT              = #0B53,
                GL_SHADE_MODEL                      = #0B54,
                GL_COLOR_MATERIAL_FACE              = #0B55,
                GL_COLOR_MATERIAL_PARAMETER         = #0B56,
                GL_COLOR_MATERIAL                   = #0B57,
                GL_FOG                              = #0B60,
                GL_FOG_INDEX                        = #0B61,
                GL_FOG_DENSITY                      = #0B62,
                GL_FOG_START                        = #0B63,
                GL_FOG_END                          = #0B64,
                GL_FOG_MODE                         = #0B65,
                GL_FOG_COLOR                        = #0B66,
                GL_DEPTH_RANGE                      = #0B70,
                GL_DEPTH_TEST                       = #0B71,
                GL_DEPTH_WRITEMASK                  = #0B72,
                GL_DEPTH_CLEAR_VALUE                = #0B73,
                GL_DEPTH_FUNC                       = #0B74,
                GL_ACCUM_CLEAR_VALUE                = #0B80,
                GL_STENCIL_TEST                     = #0B90,
                GL_STENCIL_CLEAR_VALUE              = #0B91,
                GL_STENCIL_FUNC                     = #0B92,
                GL_STENCIL_VALUE_MASK               = #0B93,
                GL_STENCIL_FAIL                     = #0B94,
                GL_STENCIL_PASS_DEPTH_FAIL          = #0B95,
                GL_STENCIL_PASS_DEPTH_PASS          = #0B96,
                GL_STENCIL_REF                      = #0B97,
                GL_STENCIL_WRITEMASK                = #0B98,
                GL_MATRIX_MODE                      = #0BA0,
                GL_NORMALIZE                        = #0BA1,
                GL_VIEWPORT                         = #0BA2,
                GL_MODELVIEW_STACK_DEPTH            = #0BA3,
                GL_PROJECTION_STACK_DEPTH           = #0BA4,
                GL_TEXTURE_STACK_DEPTH              = #0BA5,
                GL_MODELVIEW_MATRIX                 = #0BA6,
                GL_PROJECTION_MATRIX                = #0BA7,
                GL_TEXTURE_MATRIX                   = #0BA8,
                GL_ATTRIB_STACK_DEPTH               = #0BB0,
                GL_CLIENT_ATTRIB_STACK_DEPTH        = #0BB1,
                GL_ALPHA_TEST                       = #0BC0,
                GL_ALPHA_TEST_FUNC                  = #0BC1,
                GL_ALPHA_TEST_REF                   = #0BC2,
                GL_DITHER                           = #0BD0,
                GL_BLEND_DST                        = #0BE0,
                GL_BLEND_SRC                        = #0BE1,
                GL_BLEND                            = #0BE2,
                GL_LOGIC_OP_MODE                    = #0BF0,
                GL_INDEX_LOGIC_OP                   = #0BF1,
                GL_LOGIC_OP = GL_INDEX_LOGIC_OP,
                GL_COLOR_LOGIC_OP                   = #0BF2,
                GL_AUX_BUFFERS                      = #0C00,
                GL_DRAW_BUFFER                      = #0C01,
                GL_READ_BUFFER                      = #0C02,
                GL_SCISSOR_BOX                      = #0C10,
                GL_SCISSOR_TEST                     = #0C11,
                GL_INDEX_CLEAR_VALUE                = #0C20,
                GL_INDEX_WRITEMASK                  = #0C21,
                GL_COLOR_CLEAR_VALUE                = #0C22,
                GL_COLOR_WRITEMASK                  = #0C23,
                GL_INDEX_MODE                       = #0C30,
                GL_RGBA_MODE                        = #0C31,
                GL_DOUBLEBUFFER                     = #0C32,
                GL_STEREO                           = #0C33,
                GL_RENDER_MODE                      = #0C40,
                GL_PERSPECTIVE_CORRECTION_HINT      = #0C50,
                GL_POINT_SMOOTH_HINT                = #0C51,
                GL_LINE_SMOOTH_HINT                 = #0C52,
                GL_POLYGON_SMOOTH_HINT              = #0C53,
                GL_FOG_HINT                         = #0C54,
                GL_TEXTURE_GEN_S                    = #0C60,
                GL_TEXTURE_GEN_T                    = #0C61,
                GL_TEXTURE_GEN_R                    = #0C62,
                GL_TEXTURE_GEN_Q                    = #0C63,
                GL_PIXEL_MAP_I_TO_I_SIZE            = #0CB0,
                GL_PIXEL_MAP_S_TO_S_SIZE            = #0CB1,
                GL_PIXEL_MAP_I_TO_R_SIZE            = #0CB2,
                GL_PIXEL_MAP_I_TO_G_SIZE            = #0CB3,
                GL_PIXEL_MAP_I_TO_B_SIZE            = #0CB4,
                GL_PIXEL_MAP_I_TO_A_SIZE            = #0CB5,
                GL_PIXEL_MAP_R_TO_R_SIZE            = #0CB6,
                GL_PIXEL_MAP_G_TO_G_SIZE            = #0CB7,
                GL_PIXEL_MAP_B_TO_B_SIZE            = #0CB8,
                GL_PIXEL_MAP_A_TO_A_SIZE            = #0CB9,
                GL_UNPACK_SWAP_BYTES                = #0CF0,
                GL_UNPACK_LSB_FIRST                 = #0CF1,
                GL_UNPACK_ROW_LENGTH                = #0CF2,
                GL_UNPACK_SKIP_ROWS                 = #0CF3,
                GL_UNPACK_SKIP_PIXELS               = #0CF4,
                GL_UNPACK_ALIGNMENT                 = #0CF5,
                GL_PACK_SWAP_BYTES                  = #0D00,
                GL_PACK_LSB_FIRST                   = #0D01,
                GL_PACK_ROW_LENGTH                  = #0D02,
                GL_PACK_SKIP_ROWS                   = #0D03,
                GL_PACK_SKIP_PIXELS                 = #0D04,
                GL_PACK_ALIGNMENT                   = #0D05,
                GL_MAP_COLOR                        = #0D10,
                GL_MAP_STENCIL                      = #0D11,
                GL_INDEX_SHIFT                      = #0D12,
                GL_INDEX_OFFSET                     = #0D13,
                GL_RED_SCALE                        = #0D14,
                GL_RED_BIAS                         = #0D15,
                GL_ZOOM_X                           = #0D16,
                GL_ZOOM_Y                           = #0D17,
                GL_GREEN_SCALE                      = #0D18,
                GL_GREEN_BIAS                       = #0D19,
                GL_BLUE_SCALE                       = #0D1A,
                GL_BLUE_BIAS                        = #0D1B,
                GL_ALPHA_SCALE                      = #0D1C,
                GL_ALPHA_BIAS                       = #0D1D,
                GL_DEPTH_SCALE                      = #0D1E,
                GL_DEPTH_BIAS                       = #0D1F,
                GL_MAX_EVAL_ORDER                   = #0D30,
                GL_MAX_LIGHTS                       = #0D31,
                GL_MAX_CLIP_PLANES                  = #0D32,
                GL_MAX_TEXTURE_SIZE                 = #0D33,
                GL_MAX_PIXEL_MAP_TABLE              = #0D34,
                GL_MAX_ATTRIB_STACK_DEPTH           = #0D35,
                GL_MAX_MODELVIEW_STACK_DEPTH        = #0D36,
                GL_MAX_NAME_STACK_DEPTH             = #0D37,
                GL_MAX_PROJECTION_STACK_DEPTH       = #0D38,
                GL_MAX_TEXTURE_STACK_DEPTH          = #0D39,
                GL_MAX_VIEWPORT_DIMS                = #0D3A,
                GL_MAX_CLIENT_ATTRIB_STACK_DEPTH    = #0D3B,
                GL_SUBPIXEL_BITS                    = #0D50,
                GL_INDEX_BITS                       = #0D51,
                GL_RED_BITS                         = #0D52,
                GL_GREEN_BITS                       = #0D53,
                GL_BLUE_BITS                        = #0D54,
                GL_ALPHA_BITS                       = #0D55,
                GL_DEPTH_BITS                       = #0D56,
                GL_STENCIL_BITS                     = #0D57,
                GL_ACCUM_RED_BITS                   = #0D58,
                GL_ACCUM_GREEN_BITS                 = #0D59,
                GL_ACCUM_BLUE_BITS                  = #0D5A,
                GL_ACCUM_ALPHA_BITS                 = #0D5B,
                GL_NAME_STACK_DEPTH                 = #0D70,
                GL_AUTO_NORMAL                      = #0D80,
                GL_MAP1_COLOR_4                     = #0D90,
                GL_MAP1_INDEX                       = #0D91,
                GL_MAP1_NORMAL                      = #0D92,
                GL_MAP1_TEXTURE_COORD_1             = #0D93,
                GL_MAP1_TEXTURE_COORD_2             = #0D94,
                GL_MAP1_TEXTURE_COORD_3             = #0D95,
                GL_MAP1_TEXTURE_COORD_4             = #0D96,
                GL_MAP1_VERTEX_3                    = #0D97,
                GL_MAP1_VERTEX_4                    = #0D98,
                GL_MAP2_COLOR_4                     = #0DB0,
                GL_MAP2_INDEX                       = #0DB1,
                GL_MAP2_NORMAL                      = #0DB2,
                GL_MAP2_TEXTURE_COORD_1             = #0DB3,
                GL_MAP2_TEXTURE_COORD_2             = #0DB4,
                GL_MAP2_TEXTURE_COORD_3             = #0DB5,
                GL_MAP2_TEXTURE_COORD_4             = #0DB6,
                GL_MAP2_VERTEX_3                    = #0DB7,
                GL_MAP2_VERTEX_4                    = #0DB8,
                GL_MAP1_GRID_DOMAIN                 = #0DD0,
                GL_MAP1_GRID_SEGMENTS               = #0DD1,
                GL_MAP2_GRID_DOMAIN                 = #0DD2,
                GL_MAP2_GRID_SEGMENTS               = #0DD3,
                GL_TEXTURE_1D                       = #0DE0,
                GL_TEXTURE_2D                       = #0DE1,
                GL_FEEDBACK_BUFFER_POINTER          = #0DF0,
                GL_FEEDBACK_BUFFER_SIZE             = #0DF1,
                GL_FEEDBACK_BUFFER_TYPE             = #0DF2,
                GL_SELECTION_BUFFER_POINTER         = #0DF3,
                GL_SELECTION_BUFFER_SIZE            = #0DF4,
                GL_POLYGON_OFFSET_UNITS             = #2A00,
                GL_POLYGON_OFFSET_POINT             = #2A01,
                GL_POLYGON_OFFSET_LINE              = #2A02,
                GL_POLYGON_OFFSET_FILL              = #8037,
                GL_POLYGON_OFFSET_FACTOR            = #8038,
                GL_TEXTURE_BINDING_1D               = #8068,
                GL_TEXTURE_BINDING_2D               = #8069,
                GL_VERTEX_ARRAY                     = #8074,
                GL_NORMAL_ARRAY                     = #8075,
                GL_COLOR_ARRAY                      = #8076,
                GL_INDEX_ARRAY                      = #8077,
                GL_TEXTURE_COORD_ARRAY              = #8078,
                GL_EDGE_FLAG_ARRAY                  = #8079,
                GL_VERTEX_ARRAY_SIZE                = #807A,
                GL_VERTEX_ARRAY_TYPE                = #807B,
                GL_VERTEX_ARRAY_STRIDE              = #807C,
                GL_NORMAL_ARRAY_TYPE                = #807E,    
                GL_NORMAL_ARRAY_STRIDE              = #807F,
                GL_COLOR_ARRAY_SIZE                 = #8081,
                GL_COLOR_ARRAY_TYPE                 = #8082,
                GL_COLOR_ARRAY_STRIDE               = #8083,
                GL_INDEX_ARRAY_TYPE                 = #8085,
                GL_INDEX_ARRAY_STRIDE               = #8086,
                GL_TEXTURE_COORD_ARRAY_SIZE         = #8088,
                GL_TEXTURE_COORD_ARRAY_TYPE         = #8089,
                GL_TEXTURE_COORD_ARRAY_STRIDE       = #808A,
                GL_EDGE_FLAG_ARRAY_STRIDE           = #808C
--      GL_VERTEX_ARRAY_COUNT_EXT
--      GL_NORMAL_ARRAY_COUNT_EXT
--      GL_COLOR_ARRAY_COUNT_EXT
--      GL_INDEX_ARRAY_COUNT_EXT
--      GL_TEXTURE_COORD_ARRAY_COUNT_EXT
--      GL_EDGE_FLAG_ARRAY_COUNT_EXT
--      GL_ARRAY_ELEMENT_LOCK_COUNT_SGI
--      GL_ARRAY_ELEMENT_LOCK_FIRST_SGI
--      GL_INDEX_TEST_SGI
--      GL_INDEX_TEST_FUNC_SGI
--      GL_INDEX_TEST_REF_SGI
--      GL_INDEX_MATERIAL_SGI
--      GL_INDEX_MATERIAL_FACE_SGI
--      GL_INDEX_MATERIAL_PARAMETER_SGI

--/* DEV/SUG:
constant GL_1 = {
                 GL_TEXTURE_STACK_DEPTH,
                 GL_UNPACK_ALIGNMENT,
                 GL_UNPACK_LSB_FIRST,
                 GL_UNPACK_ROW_LENGTH,
                 GL_UNPACK_SKIP_PIXELS,
                 GL_UNPACK_SKIP_ROWS,
                 GL_UNPACK_SWAP_BYTES,
                 GL_VERTEX_ARRAY,
                 GL_VERTEX_ARRAY_SIZE,
                 GL_VERTEX_ARRAY_STRIDE,
                 GL_VERTEX_ARRAY_TYPE,
                 GL_ZOOM_X,
                 GL_ZOOM_Y}
         GL_4 = {
                 GL_VIEWPORT},
         GL_16 = {
                  GL_TEXTURE_MATRIX}

global constant GL_CURRENT_COLOR                    = #0B00,
                GL_CURRENT_INDEX                    = #0B01,
                GL_CURRENT_NORMAL                   = #0B02,
                GL_CURRENT_TEXTURE_COORDS           = #0B03,
                GL_CURRENT_RASTER_COLOR             = #0B04,
                GL_CURRENT_RASTER_INDEX             = #0B05,
                GL_CURRENT_RASTER_TEXTURE_COORDS    = #0B06,
                GL_CURRENT_RASTER_POSITION          = #0B07,
                GL_CURRENT_RASTER_POSITION_VALID    = #0B08,
                GL_CURRENT_RASTER_DISTANCE          = #0B09,
                GL_POINT_SMOOTH                     = #0B10,
                GL_POINT_SIZE                       = #0B11,
                GL_POINT_SIZE_RANGE                 = #0B12,
                GL_POINT_SIZE_GRANULARITY           = #0B13,
                GL_LINE_SMOOTH                      = #0B20,
                GL_LINE_WIDTH                       = #0B21,
                GL_LINE_WIDTH_RANGE                 = #0B22,
                GL_LINE_WIDTH_GRANULARITY           = #0B23,
                GL_LINE_STIPPLE                     = #0B24,
                GL_LINE_STIPPLE_PATTERN             = #0B25,
                GL_LINE_STIPPLE_REPEAT              = #0B26,
                GL_LIST_MODE                        = #0B30,
                GL_MAX_LIST_NESTING                 = #0B31,
                GL_LIST_BASE                        = #0B32,
                GL_LIST_INDEX                       = #0B33,
                GL_POLYGON_MODE                     = #0B40,
                GL_POLYGON_SMOOTH                   = #0B41,
                GL_POLYGON_STIPPLE                  = #0B42,
                GL_EDGE_FLAG                        = #0B43,
                GL_CULL_FACE                        = #0B44,
                GL_CULL_FACE_MODE                   = #0B45,
                GL_FRONT_FACE                       = #0B46,
                GL_LIGHTING                         = #0B50,
                GL_LIGHT_MODEL_LOCAL_VIEWER         = #0B51,
                GL_LIGHT_MODEL_TWO_SIDE             = #0B52,
                GL_LIGHT_MODEL_AMBIENT              = #0B53,
                GL_SHADE_MODEL                      = #0B54,
                GL_COLOR_MATERIAL_FACE              = #0B55,
                GL_COLOR_MATERIAL_PARAMETER         = #0B56,
                GL_COLOR_MATERIAL                   = #0B57,
                GL_FOG                              = #0B60,
                GL_FOG_INDEX                        = #0B61,
                GL_FOG_DENSITY                      = #0B62,
                GL_FOG_START                        = #0B63,
                GL_FOG_END                          = #0B64,
                GL_FOG_MODE                         = #0B65,
                GL_FOG_COLOR                        = #0B66,
                GL_DEPTH_RANGE                      = #0B70,
                GL_DEPTH_TEST                       = #0B71,
                GL_DEPTH_WRITEMASK                  = #0B72,
                GL_DEPTH_CLEAR_VALUE                = #0B73,
                GL_DEPTH_FUNC                       = #0B74,
                GL_ACCUM_CLEAR_VALUE                = #0B80,
                GL_STENCIL_TEST                     = #0B90,
                GL_STENCIL_CLEAR_VALUE              = #0B91,
                GL_STENCIL_FUNC                     = #0B92,
                GL_STENCIL_VALUE_MASK               = #0B93,
                GL_STENCIL_FAIL                     = #0B94,
                GL_STENCIL_PASS_DEPTH_FAIL          = #0B95,
                GL_STENCIL_PASS_DEPTH_PASS          = #0B96,
                GL_STENCIL_REF                      = #0B97,
                GL_STENCIL_WRITEMASK                = #0B98,
                GL_MATRIX_MODE                      = #0BA0,
                GL_NORMALIZE                        = #0BA1,
                GL_MODELVIEW_STACK_DEPTH            = #0BA3,
                GL_PROJECTION_STACK_DEPTH           = #0BA4,
                GL_MODELVIEW_MATRIX                 = #0BA6,
                GL_PROJECTION_MATRIX                = #0BA7,
                GL_ATTRIB_STACK_DEPTH               = #0BB0,
                GL_CLIENT_ATTRIB_STACK_DEPTH        = #0BB1,
                GL_ALPHA_TEST                       = #0BC0,
                GL_ALPHA_TEST_FUNC                  = #0BC1,
                GL_ALPHA_TEST_REF                   = #0BC2,
                GL_DITHER                           = #0BD0,
                GL_BLEND_DST                        = #0BE0,
                GL_BLEND_SRC                        = #0BE1,
                GL_BLEND                            = #0BE2,
                GL_LOGIC_OP_MODE                    = #0BF0,
                GL_INDEX_LOGIC_OP                   = #0BF1,
                GL_LOGIC_OP = GL_INDEX_LOGIC_OP,
                GL_COLOR_LOGIC_OP                   = #0BF2,
                GL_AUX_BUFFERS                      = #0C00,
                GL_DRAW_BUFFER                      = #0C01,
                GL_READ_BUFFER                      = #0C02,
                GL_SCISSOR_BOX                      = #0C10,
                GL_SCISSOR_TEST                     = #0C11,
                GL_INDEX_CLEAR_VALUE                = #0C20,
                GL_INDEX_WRITEMASK                  = #0C21,
                GL_COLOR_CLEAR_VALUE                = #0C22,
                GL_COLOR_WRITEMASK                  = #0C23,
                GL_INDEX_MODE                       = #0C30,
                GL_RGBA_MODE                        = #0C31,
                GL_DOUBLEBUFFER                     = #0C32,
                GL_STEREO                           = #0C33,
                GL_RENDER_MODE                      = #0C40,
                GL_PERSPECTIVE_CORRECTION_HINT      = #0C50,
                GL_POINT_SMOOTH_HINT                = #0C51,
                GL_LINE_SMOOTH_HINT                 = #0C52,
                GL_POLYGON_SMOOTH_HINT              = #0C53,
                GL_FOG_HINT                         = #0C54,
                GL_TEXTURE_GEN_S                    = #0C60,
                GL_TEXTURE_GEN_T                    = #0C61,
                GL_TEXTURE_GEN_R                    = #0C62,
                GL_TEXTURE_GEN_Q                    = #0C63,
                GL_PIXEL_MAP_I_TO_I_SIZE            = #0CB0,
                GL_PIXEL_MAP_S_TO_S_SIZE            = #0CB1,
                GL_PIXEL_MAP_I_TO_R_SIZE            = #0CB2,
                GL_PIXEL_MAP_I_TO_G_SIZE            = #0CB3,
                GL_PIXEL_MAP_I_TO_B_SIZE            = #0CB4,
                GL_PIXEL_MAP_I_TO_A_SIZE            = #0CB5,
                GL_PIXEL_MAP_R_TO_R_SIZE            = #0CB6,
                GL_PIXEL_MAP_G_TO_G_SIZE            = #0CB7,
                GL_PIXEL_MAP_B_TO_B_SIZE            = #0CB8,
                GL_PIXEL_MAP_A_TO_A_SIZE            = #0CB9,
                GL_PACK_SWAP_BYTES                  = #0D00,
                GL_PACK_LSB_FIRST                   = #0D01,
                GL_PACK_ROW_LENGTH                  = #0D02,
                GL_PACK_SKIP_ROWS                   = #0D03,
                GL_PACK_SKIP_PIXELS                 = #0D04,
                GL_PACK_ALIGNMENT                   = #0D05,
                GL_MAP_COLOR                        = #0D10,
                GL_MAP_STENCIL                      = #0D11,
                GL_INDEX_SHIFT                      = #0D12,
                GL_INDEX_OFFSET                     = #0D13,
                GL_RED_SCALE                        = #0D14,
                GL_RED_BIAS                         = #0D15,
                GL_GREEN_SCALE                      = #0D18,
                GL_GREEN_BIAS                       = #0D19,
                GL_BLUE_SCALE                       = #0D1A,
                GL_BLUE_BIAS                        = #0D1B,
                GL_ALPHA_SCALE                      = #0D1C,
                GL_ALPHA_BIAS                       = #0D1D,
                GL_DEPTH_SCALE                      = #0D1E,
                GL_DEPTH_BIAS                       = #0D1F,
                GL_MAX_EVAL_ORDER                   = #0D30,
                GL_MAX_LIGHTS                       = #0D31,
                GL_MAX_CLIP_PLANES                  = #0D32,
                GL_MAX_TEXTURE_SIZE                 = #0D33,
                GL_MAX_PIXEL_MAP_TABLE              = #0D34,
                GL_MAX_ATTRIB_STACK_DEPTH           = #0D35,
                GL_MAX_MODELVIEW_STACK_DEPTH        = #0D36,
                GL_MAX_NAME_STACK_DEPTH             = #0D37,
                GL_MAX_PROJECTION_STACK_DEPTH       = #0D38,
                GL_MAX_TEXTURE_STACK_DEPTH          = #0D39,
                GL_MAX_VIEWPORT_DIMS                = #0D3A,
                GL_MAX_CLIENT_ATTRIB_STACK_DEPTH    = #0D3B,
                GL_SUBPIXEL_BITS                    = #0D50,
                GL_INDEX_BITS                       = #0D51,
                GL_RED_BITS                         = #0D52,
                GL_GREEN_BITS                       = #0D53,
                GL_BLUE_BITS                        = #0D54,
                GL_ALPHA_BITS                       = #0D55,
                GL_DEPTH_BITS                       = #0D56,
                GL_STENCIL_BITS                     = #0D57,
                GL_ACCUM_RED_BITS                   = #0D58,
                GL_ACCUM_GREEN_BITS                 = #0D59,
                GL_ACCUM_BLUE_BITS                  = #0D5A,
                GL_ACCUM_ALPHA_BITS                 = #0D5B,
                GL_NAME_STACK_DEPTH                 = #0D70,
                GL_AUTO_NORMAL                      = #0D80,
                GL_MAP1_COLOR_4                     = #0D90,
                GL_MAP1_INDEX                       = #0D91,
                GL_MAP1_NORMAL                      = #0D92,
                GL_MAP1_TEXTURE_COORD_1             = #0D93,
                GL_MAP1_TEXTURE_COORD_2             = #0D94,
                GL_MAP1_TEXTURE_COORD_3             = #0D95,
                GL_MAP1_TEXTURE_COORD_4             = #0D96,
                GL_MAP1_VERTEX_3                    = #0D97,
                GL_MAP1_VERTEX_4                    = #0D98,
                GL_MAP2_COLOR_4                     = #0DB0,
                GL_MAP2_INDEX                       = #0DB1,
                GL_MAP2_NORMAL                      = #0DB2,
                GL_MAP2_TEXTURE_COORD_1             = #0DB3,
                GL_MAP2_TEXTURE_COORD_2             = #0DB4,
                GL_MAP2_TEXTURE_COORD_3             = #0DB5,
                GL_MAP2_TEXTURE_COORD_4             = #0DB6,
                GL_MAP2_VERTEX_3                    = #0DB7,
                GL_MAP2_VERTEX_4                    = #0DB8,
                GL_MAP1_GRID_DOMAIN                 = #0DD0,
                GL_MAP1_GRID_SEGMENTS               = #0DD1,
                GL_MAP2_GRID_DOMAIN                 = #0DD2,
                GL_MAP2_GRID_SEGMENTS               = #0DD3,
                GL_TEXTURE_1D                       = #0DE0,
                GL_TEXTURE_2D                       = #0DE1,
                GL_FEEDBACK_BUFFER_POINTER          = #0DF0,
                GL_FEEDBACK_BUFFER_SIZE             = #0DF1,
                GL_FEEDBACK_BUFFER_TYPE             = #0DF2,
                GL_SELECTION_BUFFER_POINTER         = #0DF3,
                GL_SELECTION_BUFFER_SIZE            = #0DF4,
                GL_POLYGON_OFFSET_UNITS             = #2A00,
                GL_POLYGON_OFFSET_POINT             = #2A01,
                GL_POLYGON_OFFSET_LINE              = #2A02,
                GL_POLYGON_OFFSET_FILL              = #8037,
                GL_POLYGON_OFFSET_FACTOR            = #8038,
                GL_TEXTURE_BINDING_1D               = #8068,
                GL_TEXTURE_BINDING_2D               = #8069,
                GL_NORMAL_ARRAY                     = #8075,
                GL_COLOR_ARRAY                      = #8076,
                GL_INDEX_ARRAY                      = #8077,
                GL_TEXTURE_COORD_ARRAY              = #8078,
                GL_EDGE_FLAG_ARRAY                  = #8079,
                GL_NORMAL_ARRAY_TYPE                = #807E,    
                GL_NORMAL_ARRAY_STRIDE              = #807F,
                GL_COLOR_ARRAY_SIZE                 = #8081,
                GL_COLOR_ARRAY_TYPE                 = #8082,
                GL_COLOR_ARRAY_STRIDE               = #8083,
                GL_INDEX_ARRAY_TYPE                 = #8085,
                GL_INDEX_ARRAY_STRIDE               = #8086,
                GL_TEXTURE_COORD_ARRAY_SIZE         = #8088,
                GL_TEXTURE_COORD_ARRAY_TYPE         = #8089,
                GL_TEXTURE_COORD_ARRAY_STRIDE       = #808A,
                GL_EDGE_FLAG_ARRAY_STRIDE           = #808C
--      GL_VERTEX_ARRAY_COUNT_EXT
--      GL_NORMAL_ARRAY_COUNT_EXT
--      GL_COLOR_ARRAY_COUNT_EXT
--      GL_INDEX_ARRAY_COUNT_EXT
--      GL_TEXTURE_COORD_ARRAY_COUNT_EXT
--      GL_EDGE_FLAG_ARRAY_COUNT_EXT
--      GL_ARRAY_ELEMENT_LOCK_COUNT_SGI
--      GL_ARRAY_ELEMENT_LOCK_FIRST_SGI
--      GL_INDEX_TEST_SGI
--      GL_INDEX_TEST_FUNC_SGI
--      GL_INDEX_TEST_REF_SGI
--      GL_INDEX_MATERIAL_SGI
--      GL_INDEX_MATERIAL_FACE_SGI
--      GL_INDEX_MATERIAL_PARAMETER_SGI
--*/

-->

--DEV use the routines in pGUI.e:
global 
function validate_proc(atom lib, sequence name, sequence parms)
    integer hProc

    hProc = define_c_proc(lib,name,parms)
    if hProc = -1 then
        puts(1,"Error! Can\'t find "&name&"().\nPress any key..")
        while get_key() =-1 do
        end while
        abort(1)
    end if
    return hProc
end function


global 
function validate_func(atom lib, sequence name, sequence parms, atom rtype)
    integer hFunc

    hFunc = define_c_func(lib,name,parms,rtype)
    if hFunc = -1 then
        puts(1,"Error! Can\'t find "&name&"().\nPress any key..")
        while get_key() =-1 do
        end while
        abort(1)
    end if
    return hFunc
end function

global 
constant GL_LIBPATH = "/usr/lib/x86_64-linux-gnu/"

--define some GL types
global constant GLbyte      = C_CHAR,
                GLubyte     = C_UCHAR,
                GLshort     = C_SHORT,
                GLushort    = C_USHORT,
                GLenum      = C_UINT,
                GLint       = C_INT,
                GLuint      = C_UINT,
                GLsizei     = C_INT,
                GLbitfield  = C_UINT,
                GLfloat     = C_FLOAT,
                GLclampf    = C_FLOAT,
                GLdouble    = C_DOUBLE
--/GL types

--************** Define OpenGL constants ****************

-- Extensions
global constant GL_VERSION_1_1 = 1
global constant GL_EXT_abgr = 1
global constant GL_EXT_bgra = 1
global constant GL_EXT_packed_pixels = 1
global constant GL_EXT_paletted_texture = 1
global constant GL_EXT_vertex_array = 1
global constant GL_SGI_compiled_vertex_array = 1
global constant GL_SGI_cull_vertex = 1
global constant GL_SGI_index_array_formats = 1
global constant GL_SGI_index_func = 1
global constant GL_SGI_index_material = 1
global constant GL_SGI_index_texture = 1
global constant GL_WIN_swap_hint = 1

-- ClientAttribMask
global constant GL_CLIENT_PIXEL_STORE_BIT = #00000001
global constant GL_CLIENT_VERTEX_ARRAY_BIT = #00000002
global constant GL_CLIENT_ALL_ATTRIB_BITS = #FFFFFFFF

-- Boolean
global constant GL_FALSE = 0
global constant GL_TRUE = 1

-- AccumOp
global constant GL_ACCUM = #0100
global constant GL_LOAD = #0101
global constant GL_RETURN = #0102
global constant GL_MULT = #0103
global constant GL_ADD = #0104

-- AlphaFunction
global constant GL_NEVER = #0200
global constant GL_LESS = #0201
global constant GL_EQUAL = #0202
global constant GL_LEQUAL = #0203
global constant GL_GREATER = #0204
global constant GL_NOTEQUAL = #0205
global constant GL_GEQUAL = #0206
global constant GL_ALWAYS = #0207

-- BlendingFactorDest
global constant GL_ZERO = 0
global constant GL_ONE = 1
global constant GL_SRC_COLOR = #0300
global constant GL_ONE_MINUS_SRC_COLOR = #0301
global constant GL_SRC_ALPHA = #0302
global constant GL_ONE_MINUS_SRC_ALPHA = #0303
global constant GL_DST_ALPHA = #0304
global constant GL_ONE_MINUS_DST_ALPHA = #0305

-- BlendingFactorSrc
--      GL_ZERO
--      GL_ONE
global constant GL_DST_COLOR = #0306
global constant GL_ONE_MINUS_DST_COLOR = #0307
global constant GL_SRC_ALPHA_SATURATE = #0308
--      GL_SRC_ALPHA
--      GL_ONE_MINUS_SRC_ALPHA
--      GL_DST_ALPHA
--      GL_ONE_MINUS_DST_ALPHA

-- ColorMaterialFace
--      GL_FRONT
--      GL_BACK
--      GL_FRONT_AND_BACK

-- ColorMaterialParameter
--      GL_AMBIENT
--      GL_DIFFUSE
--      GL_SPECULAR
--      GL_EMISSION
--      GL_AMBIENT_AND_DIFFUSE

-- ColorPointerType
--      GL_BYTE
--      GL_UNSIGNED_BYTE
--      GL_SHORT
--      GL_UNSIGNED_SHORT
--      GL_INT
--      GL_UNSIGNED_INT
--      GL_FLOAT
--      GL_DOUBLE

-- CullFaceMode
--      GL_FRONT
--      GL_BACK
--      GL_FRONT_AND_BACK

-- CullParameterSGI
--      GL_CULL_VERTEX_EYE_POSITION_SGI
--      GL_CULL_VERTEX_OBJECT_POSITION_SGI

-- DepthFunction
--      GL_NEVER
--      GL_LESS
--      GL_EQUAL
--      GL_LEQUAL
--      GL_GREATER
--      GL_NOTEQUAL
--      GL_GEQUAL
--      GL_ALWAYS

-- DrawBufferMode
global constant GL_NONE = 0
global constant GL_FRONT_LEFT = #0400
global constant GL_FRONT_RIGHT = #0401
global constant GL_BACK_LEFT = #0402
global constant GL_BACK_RIGHT = #0403
global constant GL_FRONT = #0404
global constant GL_BACK = #0405
global constant GL_LEFT = #0406
global constant GL_RIGHT = #0407
global constant GL_FRONT_AND_BACK = #0408
global constant GL_AUX0 = #0409
global constant GL_AUX1 = #040A
global constant GL_AUX2 = #040B
global constant GL_AUX3 = #040C

-- FeedbackType
global constant GL_2D = #0600
global constant GL_3D = #0601
global constant GL_3D_COLOR = #0602
global constant GL_3D_COLOR_TEXTURE = #0603
global constant GL_4D_COLOR_TEXTURE = #0604

-- FeedBackToken
global constant GL_PASS_THROUGH_TOKEN = #0700
global constant GL_POINT_TOKEN = #0701
global constant GL_LINE_TOKEN = #0702
global constant GL_POLYGON_TOKEN = #0703
global constant GL_BITMAP_TOKEN = #0704
global constant GL_DRAW_PIXEL_TOKEN = #0705
global constant GL_COPY_PIXEL_TOKEN = #0706
global constant GL_LINE_RESET_TOKEN = #0707

-- FogMode
--      GL_LINEAR
global constant GL_EXP = #0800
global constant GL_EXP2 = #0801

-- FogParameter
--      GL_FOG_COLOR
--      GL_FOG_DENSITY
--      GL_FOG_END
--      GL_FOG_INDEX
--      GL_FOG_MODE
--      GL_FOG_START

-- FrontFaceDirection
global constant GL_CW = #0900
global constant GL_CCW = #0901

-- GetColorTableParameterPNameEXT
--      GL_COLOR_TABLE_FORMAT_EXT
--      GL_COLOR_TABLE_WIDTH_EXT
--      GL_COLOR_TABLE_RED_SIZE_EXT
--      GL_COLOR_TABLE_GREEN_SIZE_EXT
--      GL_COLOR_TABLE_BLUE_SIZE_EXT
--      GL_COLOR_TABLE_ALPHA_SIZE_EXT
--      GL_COLOR_TABLE_LUMINANCE_SIZE_EXT
--      GL_COLOR_TABLE_INTENSITY_SIZE_EXT

-- GetMapQuery
global constant GL_COEFF = #0A00
global constant GL_ORDER = #0A01
global constant GL_DOMAIN = #0A02

-- GetPixelMap
global constant GL_PIXEL_MAP_I_TO_I = #0C70
global constant GL_PIXEL_MAP_S_TO_S = #0C71
global constant GL_PIXEL_MAP_I_TO_R = #0C72
global constant GL_PIXEL_MAP_I_TO_G = #0C73
global constant GL_PIXEL_MAP_I_TO_B = #0C74
global constant GL_PIXEL_MAP_I_TO_A = #0C75
global constant GL_PIXEL_MAP_R_TO_R = #0C76
global constant GL_PIXEL_MAP_G_TO_G = #0C77
global constant GL_PIXEL_MAP_B_TO_B = #0C78
global constant GL_PIXEL_MAP_A_TO_A = #0C79

-- GetPointervPName
global constant GL_VERTEX_ARRAY_POINTER = #808E
global constant GL_NORMAL_ARRAY_POINTER = #808F
global constant GL_COLOR_ARRAY_POINTER = #8090
global constant GL_INDEX_ARRAY_POINTER = #8091
global constant GL_TEXTURE_COORD_ARRAY_POINTER = #8092
global constant GL_EDGE_FLAG_ARRAY_POINTER = #8093

-- GetTextureParameter
--      GL_TEXTURE_MAG_FILTER
--      GL_TEXTURE_MIN_FILTER
--      GL_TEXTURE_WRAP_S
--      GL_TEXTURE_WRAP_T
global constant GL_TEXTURE_WIDTH = #1000
global constant GL_TEXTURE_HEIGHT = #1001
global constant GL_TEXTURE_INTERNAL_FORMAT = #1003
global constant GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT
global constant GL_TEXTURE_BORDER_COLOR = #1004
global constant GL_TEXTURE_BORDER = #1005
global constant GL_TEXTURE_RED_SIZE = #805C
global constant GL_TEXTURE_GREEN_SIZE = #805D
global constant GL_TEXTURE_BLUE_SIZE = #805E
global constant GL_TEXTURE_ALPHA_SIZE = #805F
global constant GL_TEXTURE_LUMINANCE_SIZE = #8060
global constant GL_TEXTURE_INTENSITY_SIZE = #8061
global constant GL_TEXTURE_PRIORITY = #8066
global constant GL_TEXTURE_RESIDENT = #8067

-- HintMode
global constant GL_DONT_CARE = #1100
global constant GL_FASTEST = #1101
global constant GL_NICEST = #1102

-- HintTarget
--      GL_PERSPECTIVE_CORRECTION_HINT
--      GL_POINT_SMOOTH_HINT
--      GL_LINE_SMOOTH_HINT
--      GL_POLYGON_SMOOTH_HINT
--      GL_FOG_HINT

-- IndexMaterialParameterSGI
--      GL_INDEX_OFFSET

-- IndexPointerType
--      GL_SHORT
--      GL_INT
--      GL_FLOAT
--      GL_DOUBLE

-- IndexFunctionSGI
--      GL_NEVER
--      GL_LESS
--      GL_EQUAL
--      GL_LEQUAL
--      GL_GREATER
--      GL_NOTEQUAL
--      GL_GEQUAL
--      GL_ALWAYS

-- LightModelParameter
--      GL_LIGHT_MODEL_AMBIENT
--      GL_LIGHT_MODEL_LOCAL_VIEWER
--      GL_LIGHT_MODEL_TWO_SIDE

-- ListMode
global constant GL_COMPILE = #1300
global constant GL_COMPILE_AND_EXECUTE = #1301

-- DataType
global constant GL_BYTE = #1400
global constant GL_UNSIGNED_BYTE = #1401
global constant GL_SHORT = #1402
global constant GL_UNSIGNED_SHORT = #1403
global constant GL_INT = #1404
global constant GL_UNSIGNED_INT = #1405
global constant GL_FLOAT = #1406
global constant GL_2_BYTES = #1407
global constant GL_3_BYTES = #1408
global constant GL_4_BYTES = #1409
global constant GL_DOUBLE = #140A
global constant GL_DOUBLE_EXT = #140A

-- ListNameType
--      GL_BYTE
--      GL_UNSIGNED_BYTE
--      GL_SHORT
--      GL_UNSIGNED_SHORT
--      GL_INT
--      GL_UNSIGNED_INT
--      GL_FLOAT
--      GL_2_BYTES
--      GL_3_BYTES
--      GL_4_BYTES

-- LogicOp
global constant GL_CLEAR = #1500
global constant GL_AND = #1501
global constant GL_AND_REVERSE = #1502
global constant GL_COPY = #1503
global constant GL_AND_INVERTED = #1504
global constant GL_NOOP = #1505
global constant GL_XOR = #1506
global constant GL_OR = #1507
global constant GL_NOR = #1508
global constant GL_EQUIV = #1509
global constant GL_INVERT = #150A
global constant GL_OR_REVERSE = #150B
global constant GL_COPY_INVERTED = #150C
global constant GL_OR_INVERTED = #150D
global constant GL_NAND = #150E
global constant GL_SET = #150F

-- MapTarget
--      GL_MAP1_COLOR_4
--      GL_MAP1_INDEX
--      GL_MAP1_NORMAL
--      GL_MAP1_TEXTURE_COORD_1
--      GL_MAP1_TEXTURE_COORD_2
--      GL_MAP1_TEXTURE_COORD_3
--      GL_MAP1_TEXTURE_COORD_4
--      GL_MAP1_VERTEX_3
--      GL_MAP1_VERTEX_4
--      GL_MAP2_COLOR_4
--      GL_MAP2_INDEX
--      GL_MAP2_NORMAL
--      GL_MAP2_TEXTURE_COORD_1
--      GL_MAP2_TEXTURE_COORD_2
--      GL_MAP2_TEXTURE_COORD_3
--      GL_MAP2_TEXTURE_COORD_4
--      GL_MAP2_VERTEX_3
--      GL_MAP2_VERTEX_4

-- MaterialFace
--      GL_FRONT
--      GL_BACK
--      GL_FRONT_AND_BACK

-- MaterialParameter
global constant GL_EMISSION = #1600
global constant GL_SHININESS = #1601
global constant GL_AMBIENT_AND_DIFFUSE = #1602
global constant GL_COLOR_INDEXES = #1603
--      GL_AMBIENT
--      GL_DIFFUSE
--      GL_SPECULAR

-- MeshMode1
--      GL_POINT
--      GL_LINE

-- MeshMode2
--      GL_POINT
--      GL_LINE
--      GL_FILL

-- NormalPointerType
--      GL_BYTE
--      GL_SHORT
--      GL_INT
--      GL_FLOAT
--      GL_DOUBLE

-- PixelCopyType
global constant GL_COLOR = #1800
global constant GL_DEPTH = #1801
global constant GL_STENCIL = #1802

-- PixelFormat
global constant GL_COLOR_INDEX = #1900
global constant GL_STENCIL_INDEX = #1901
global constant GL_DEPTH_COMPONENT = #1902
global constant GL_RED = #1903
global constant GL_GREEN = #1904
global constant GL_BLUE = #1905
global constant GL_ALPHA = #1906
global constant GL_RGB = #1907
global constant GL_RGBA = #1908
global constant GL_LUMINANCE = #1909
global constant GL_LUMINANCE_ALPHA = #190A
--      GL_ABGR_EXT
--      GL_BGR_EXT
--      GL_BGRA_EXT

-- PixelMap
--      GL_PIXEL_MAP_I_TO_I
--      GL_PIXEL_MAP_S_TO_S
--      GL_PIXEL_MAP_I_TO_R
--      GL_PIXEL_MAP_I_TO_G
--      GL_PIXEL_MAP_I_TO_B
--      GL_PIXEL_MAP_I_TO_A
--      GL_PIXEL_MAP_R_TO_R
--      GL_PIXEL_MAP_G_TO_G
--      GL_PIXEL_MAP_B_TO_B
--      GL_PIXEL_MAP_A_TO_A

-- PixelStoreParameter
--      GL_UNPACK_SWAP_BYTES
--      GL_UNPACK_LSB_FIRST
--      GL_UNPACK_ROW_LENGTH
--      GL_UNPACK_SKIP_ROWS
--      GL_UNPACK_SKIP_PIXELS
--      GL_UNPACK_ALIGNMENT
--      GL_PACK_SWAP_BYTES
--      GL_PACK_LSB_FIRST
--      GL_PACK_ROW_LENGTH
--      GL_PACK_SKIP_ROWS
--      GL_PACK_SKIP_PIXELS
--      GL_PACK_ALIGNMENT

-- PixelTransferParameter
--      GL_MAP_COLOR
--      GL_MAP_STENCIL
--      GL_INDEX_SHIFT
--      GL_INDEX_OFFSET
--      GL_RED_SCALE
--      GL_RED_BIAS
--      GL_GREEN_SCALE
--      GL_GREEN_BIAS
--      GL_BLUE_SCALE
--      GL_BLUE_BIAS
--      GL_ALPHA_SCALE
--      GL_ALPHA_BIAS
--      GL_DEPTH_SCALE
--      GL_DEPTH_BIAS

-- PixelType
global constant GL_BITMAP = #1A00
--      GL_BYTE
--      GL_UNSIGNED_BYTE
--      GL_SHORT
--      GL_UNSIGNED_SHORT
--      GL_INT
--      GL_UNSIGNED_INT
--      GL_FLOAT
--      GL_UNSIGNED_BYTE_3_3_2_EXT
--      GL_UNSIGNED_SHORT_4_4_4_4_EXT
--      GL_UNSIGNED_SHORT_5_5_5_1_EXT
--      GL_UNSIGNED_INT_8_8_8_8_EXT
--      GL_UNSIGNED_INT_10_10_10_2_EXT

-- PolygonMode
global constant GL_POINT = #1B00,
                GL_LINE  = #1B01,
                GL_FILL  = #1B02

-- ReadBufferMode
--      GL_FRONT_LEFT
--      GL_FRONT_RIGHT
--      GL_BACK_LEFT
--      GL_BACK_RIGHT
--      GL_FRONT
--      GL_BACK
--      GL_LEFT
--      GL_RIGHT
--      GL_AUX0
--      GL_AUX1
--      GL_AUX2
--      GL_AUX3

-- RenderingMode
global constant GL_RENDER = #1C00
global constant GL_FEEDBACK = #1C01
global constant GL_SELECT = #1C02

-- StencilFunction
--      GL_NEVER
--      GL_LESS
--      GL_EQUAL
--      GL_LEQUAL
--      GL_GREATER
--      GL_NOTEQUAL
--      GL_GEQUAL
--      GL_ALWAYS

-- StencilOp
--      GL_ZERO
global constant GL_KEEP = #1E00
global constant GL_REPLACE = #1E01
global constant GL_INCR = #1E02
global constant GL_DECR = #1E03
--      GL_INVERT

-- TexCoordPointerType
--      GL_SHORT
--      GL_INT
--      GL_FLOAT
--      GL_DOUBLE

-- TextureCoordName
global constant GL_S = #2000
global constant GL_T = #2001
global constant GL_R = #2002
global constant GL_Q = #2003

-- TextureEnvMode
global constant GL_MODULATE = #2100
global constant GL_DECAL = #2101
--      GL_BLEND
--      GL_REPLACE
--      GL_ADD

-- TextureEnvParameter
global constant GL_TEXTURE_ENV_MODE = #2200
global constant GL_TEXTURE_ENV_COLOR = #2201

-- TextureEnvTarget
global constant GL_TEXTURE_ENV = #2300

-- TextureGenMode
global constant GL_EYE_LINEAR = #2400
global constant GL_OBJECT_LINEAR = #2401
global constant GL_SPHERE_MAP = #2402

-- TextureGenParameter
global constant GL_TEXTURE_GEN_MODE = #2500
global constant GL_OBJECT_PLANE = #2501
global constant GL_EYE_PLANE = #2502

-- TextureMagFilter
global constant GL_NEAREST = #2600
global constant GL_LINEAR = #2601

-- TextureMinFilter
--      GL_NEAREST
--      GL_LINEAR
global constant GL_NEAREST_MIPMAP_NEAREST = #2700
global constant GL_LINEAR_MIPMAP_NEAREST = #2701
global constant GL_NEAREST_MIPMAP_LINEAR = #2702
global constant GL_LINEAR_MIPMAP_LINEAR = #2703

-- TextureParameterName
global constant GL_TEXTURE_MAG_FILTER = #2800
global constant GL_TEXTURE_MIN_FILTER = #2801
global constant GL_TEXTURE_WRAP_S = #2802
global constant GL_TEXTURE_WRAP_T = #2803
--      GL_TEXTURE_BORDER_COLOR
--      GL_TEXTURE_PRIORITY

-- TextureTarget
--      GL_TEXTURE_1D
--      GL_TEXTURE_2D
global constant GL_PROXY_TEXTURE_1D = #8063
global constant GL_PROXY_TEXTURE_2D = #8064

-- TextureWrapMode
global constant GL_CLAMP = #2900
global constant GL_REPEAT = #2901

-- PixelInternalFormat
global constant GL_R3_G3_B2 = #2A10
global constant GL_ALPHA4 = #803B
global constant GL_ALPHA8 = #803C
global constant GL_ALPHA12 = #803D
global constant GL_ALPHA16 = #803E
global constant GL_LUMINANCE4 = #803F
global constant GL_LUMINANCE8 = #8040
global constant GL_LUMINANCE12 = #8041
global constant GL_LUMINANCE16 = #8042
global constant GL_LUMINANCE4_ALPHA4 = #8043
global constant GL_LUMINANCE6_ALPHA2 = #8044
global constant GL_LUMINANCE8_ALPHA8 = #8045
global constant GL_LUMINANCE12_ALPHA4 = #8046
global constant GL_LUMINANCE12_ALPHA12 = #8047
global constant GL_LUMINANCE16_ALPHA16 = #8048
global constant GL_INTENSITY = #8049
global constant GL_INTENSITY4 = #804A
global constant GL_INTENSITY8 = #804B
global constant GL_INTENSITY12 = #804C
global constant GL_INTENSITY16 = #804D
global constant GL_RGB4 = #804F
global constant GL_RGB5 = #8050
global constant GL_RGB8 = #8051
global constant GL_RGB10 = #8052
global constant GL_RGB12 = #8053
global constant GL_RGB16 = #8054
global constant GL_RGBA2 = #8055
global constant GL_RGBA4 = #8056
global constant GL_RGB5_A1 = #8057
global constant GL_RGBA8 = #8058
global constant GL_RGB10_A2 = #8059
global constant GL_RGBA12 = #805A
global constant GL_RGBA16 = #805B
--      GL_COLOR_INDEX1_EXT
--      GL_COLOR_INDEX2_EXT
--      GL_COLOR_INDEX4_EXT
--      GL_COLOR_INDEX8_EXT
--      GL_COLOR_INDEX12_EXT
--      GL_COLOR_INDEX16_EXT

-- InterleavedArrayFormat
global constant GL_V2F = #2A20
global constant GL_V3F = #2A21
global constant GL_C4UB_V2F = #2A22
global constant GL_C4UB_V3F = #2A23
global constant GL_C3F_V3F = #2A24
global constant GL_N3F_V3F = #2A25
global constant GL_C4F_N3F_V3F = #2A26
global constant GL_T2F_V3F = #2A27
global constant GL_T4F_V4F = #2A28
global constant GL_T2F_C4UB_V3F = #2A29
global constant GL_T2F_C3F_V3F = #2A2A
global constant GL_T2F_N3F_V3F = #2A2B
global constant GL_T2F_C4F_N3F_V3F = #2A2C
global constant GL_T4F_C4F_N3F_V4F = #2A2D
--      GL_IUI_V2F_SGI
--      GL_IUI_V3F_SGI
--      GL_IUI_N3F_V2F_SGI
--      GL_IUI_N3F_V3F_SGI
--      GL_T2F_IUI_V2F_SGI
--      GL_T2F_IUI_V3F_SGI
--      GL_T2F_IUI_N3F_V2F_SGI
--      GL_T2F_IUI_N3F_V3F_SGI

-- VertexPointerType
--      GL_SHORT
--      GL_INT
--      GL_FLOAT
--      GL_DOUBLE

-- ClipPlaneName
global constant GL_CLIP_PLANE0 = #3000
global constant GL_CLIP_PLANE1 = #3001
global constant GL_CLIP_PLANE2 = #3002
global constant GL_CLIP_PLANE3 = #3003
global constant GL_CLIP_PLANE4 = #3004
global constant GL_CLIP_PLANE5 = #3005

-- EXT_abgr
global constant GL_ABGR_EXT = #8000

-- EXT_packed_pixels
global constant GL_UNSIGNED_BYTE_3_3_2_EXT = #8032
global constant GL_UNSIGNED_SHORT_4_4_4_4_EXT = #8033
global constant GL_UNSIGNED_SHORT_5_5_5_1_EXT = #8034
global constant GL_UNSIGNED_INT_8_8_8_8_EXT = #8035
global constant GL_UNSIGNED_INT_10_10_10_2_EXT = #8036

-- EXT_vertex_array
global constant GL_VERTEX_ARRAY_EXT = #8074
global constant GL_NORMAL_ARRAY_EXT = #8075
global constant GL_COLOR_ARRAY_EXT = #8076
global constant GL_INDEX_ARRAY_EXT = #8077
global constant GL_TEXTURE_COORD_ARRAY_EXT = #8078
global constant GL_EDGE_FLAG_ARRAY_EXT = #8079
global constant GL_VERTEX_ARRAY_SIZE_EXT = #807A
global constant GL_VERTEX_ARRAY_TYPE_EXT = #807B
global constant GL_VERTEX_ARRAY_STRIDE_EXT = #807C
global constant GL_VERTEX_ARRAY_COUNT_EXT = #807D
global constant GL_NORMAL_ARRAY_TYPE_EXT = #807E
global constant GL_NORMAL_ARRAY_STRIDE_EXT = #807F
global constant GL_NORMAL_ARRAY_COUNT_EXT = #8080
global constant GL_COLOR_ARRAY_SIZE_EXT = #8081
global constant GL_COLOR_ARRAY_TYPE_EXT = #8082
global constant GL_COLOR_ARRAY_STRIDE_EXT = #8083
global constant GL_COLOR_ARRAY_COUNT_EXT = #8084
global constant GL_INDEX_ARRAY_TYPE_EXT = #8085
global constant GL_INDEX_ARRAY_STRIDE_EXT = #8086
global constant GL_INDEX_ARRAY_COUNT_EXT = #8087
global constant GL_TEXTURE_COORD_ARRAY_SIZE_EXT = #8088
global constant GL_TEXTURE_COORD_ARRAY_TYPE_EXT = #8089
global constant GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = #808A
global constant GL_TEXTURE_COORD_ARRAY_COUNT_EXT = #808B
global constant GL_EDGE_FLAG_ARRAY_STRIDE_EXT = #808C
global constant GL_EDGE_FLAG_ARRAY_COUNT_EXT = #808D
global constant GL_VERTEX_ARRAY_POINTER_EXT = #808E
global constant GL_NORMAL_ARRAY_POINTER_EXT = #808F
global constant GL_COLOR_ARRAY_POINTER_EXT = #8090
global constant GL_INDEX_ARRAY_POINTER_EXT = #8091
global constant GL_TEXTURE_COORD_ARRAY_POINTER_EXT = #8092
global constant GL_EDGE_FLAG_ARRAY_POINTER_EXT = #8093

-- EXT_color_table
global constant GL_TABLE_TOO_LARGE_EXT = #8031
global constant GL_COLOR_TABLE_FORMAT_EXT = #80D8
global constant GL_COLOR_TABLE_WIDTH_EXT = #80D9
global constant GL_COLOR_TABLE_RED_SIZE_EXT = #80DA
global constant GL_COLOR_TABLE_GREEN_SIZE_EXT = #80DB
global constant GL_COLOR_TABLE_BLUE_SIZE_EXT = #80DC
global constant GL_COLOR_TABLE_ALPHA_SIZE_EXT = #80DD
global constant GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = #80DE
global constant GL_COLOR_TABLE_INTENSITY_SIZE_EXT = #80DF

-- EXT_bgra
global constant GL_BGR_EXT = #80E0

global constant GL_BGRA_EXT = #80E1

-- EXT_paletted_texture
global constant GL_COLOR_INDEX1_EXT = #80E2
global constant GL_COLOR_INDEX2_EXT = #80E3
global constant GL_COLOR_INDEX4_EXT = #80E4
global constant GL_COLOR_INDEX8_EXT = #80E5
global constant GL_COLOR_INDEX12_EXT = #80E6
global constant GL_COLOR_INDEX16_EXT = #80E7

-- SGI_compiled_vertex_array
global constant GL_ARRAY_ELEMENT_LOCK_FIRST_SGI = #81A8
global constant GL_ARRAY_ELEMENT_LOCK_COUNT_SGI = #81A9

-- SGI_cull_vertex
global constant GL_CULL_VERTEX_SGI = #81AA
global constant GL_CULL_VERTEX_EYE_POSITION_SGI = #81AB
global constant GL_CULL_VERTEX_OBJECT_POSITION_SGI = #81AC

-- SGI_index_array_formats
global constant GL_IUI_V2F_SGI = #81AD
global constant GL_IUI_V3F_SGI = #81AE
global constant GL_IUI_N3F_V2F_SGI = #81AF
global constant GL_IUI_N3F_V3F_SGI = #81B0
global constant GL_T2F_IUI_V2F_SGI = #81B1
global constant GL_T2F_IUI_V3F_SGI = #81B2
global constant GL_T2F_IUI_N3F_V2F_SGI = #81B3
global constant GL_T2F_IUI_N3F_V3F_SGI = #81B4

-- SGI_index_func
global constant GL_INDEX_TEST_SGI = #81B5
global constant GL_INDEX_TEST_FUNC_SGI = #81B6
global constant GL_INDEX_TEST_REF_SGI = #81B7

-- SGI_index_material
global constant GL_INDEX_MATERIAL_SGI = #81B8
global constant GL_INDEX_MATERIAL_PARAMETER_SGI = #81B9
global constant GL_INDEX_MATERIAL_FACE_SGI = #81BA

global constant GL_FOG_COORDINATE_SOURCE_EXT    = #8450
global constant GL_FOG_COORDINATE_EXT   = #8451
global constant GL_FRAGMENT_DEPTH_EXT   = #8452
global constant GL_CURRENT_FOG_COORDINATE_EXT   = #8453
global constant GL_FOG_COORDINATE_ARRAY_TYPE_EXT    = #8454
global constant GL_FOG_COORDINATE_ARRAY_STRIDE_EXT  = #8455
global constant GL_FOG_COORDINATE_ARRAY_POINTER_EXT = #8456
global constant GL_FOG_COORDINATE_ARRAY_EXT = #8457

--***********************************************************



--*************** Link functions from OpenGl32.dll **********
atom opengl32

    if platform()=LINUX then
        opengl32 = open_dll(GL_LIBPATH & "libGL.so")
    else
        opengl32 = open_dll("opengl32.dll")
    end if

    if opengl32=NULL then
        if platform()=LINUX then
            puts(1,"Error! Can\'t find " & GL_LIBPATH & "libGL.so")
        else
            puts(1,"Error! Can\'t find opengl32.dll")
        end if
    end if

constant
GlAccum                 = validate_proc(opengl32,"glAccum",{GLenum,GLfloat}),
GlBegin                 = validate_proc(opengl32,"glBegin",{C_UINT}),
GlBindTexture           = validate_proc(opengl32,"glBindTexture",{C_INT,C_UINT}),
GlBitmap                = validate_proc(opengl32,"glBitmap",{C_INT,C_INT,C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT,C_POINTER}),
GlBlendFunc             = validate_proc(opengl32,"glBlendFunc",{C_INT,C_INT}),
GlCallList              = validate_proc(opengl32,"glCallList",{C_UINT}),
GlCallLists             = validate_proc(opengl32,"glCallLists",{C_INT,C_INT,C_POINTER}),
GlClear                 = validate_proc(opengl32,"glClear",{C_UINT}),
GlClearAccum            = validate_proc(opengl32,"glClearAccum",{GLfloat,GLfloat,GLfloat,GLfloat}),
GlClearColor            = validate_proc(opengl32,"glClearColor",{GLclampf,GLclampf,GLclampf,GLclampf}),
GlClearDepth            = validate_proc(opengl32,"glClearDepth",{C_DOUBLE}),
--GlColor3b             = validate_proc(opengl32,"glColor3b",{C_CHAR,C_CHAR,C_CHAR}),
GlColor3d               = validate_proc(opengl32,"glColor3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlColor3f             = validate_proc(opengl32,"glColor3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--GlColor3i             = validate_proc(opengl32,"glColor3i",{C_INT,C_INT,C_INT}),
--GlColor4b             = validate_proc(opengl32,"glColor4b",{C_CHAR,C_CHAR,C_CHAR,C_CHAR}),
GlColor4d               = validate_proc(opengl32,"glColor4d",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlColor4f             = validate_proc(opengl32,"glColor4f",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--GlColor4i             = validate_proc(opengl32,"glColor4i",{C_INT,C_INT,C_INT,C_INT}),
GlColorPointer          = validate_proc(opengl32,"glColorPointer", {C_INT, C_UINT, C_UINT, C_POINTER}),
GlCullFace              = validate_proc(opengl32,"glCullFace",{C_INT}),
GlDeleteLists           = validate_proc(opengl32,"glDeleteLists",{C_INT,C_INT}),
GlDepthFunc             = validate_proc(opengl32,"glDepthFunc",{C_INT}),
GlDepthMask             = validate_proc(opengl32,"glDepthMask", {C_CHAR}),
GlDisable               = validate_proc(opengl32,"glDisable",{C_UINT}),
GlDisableClientState    = validate_proc(opengl32,"glDisableClientState", {C_UINT}),
GlDrawElements          = validate_proc(opengl32,"glDrawElements", {C_UINT, C_INT, C_UINT, C_POINTER}),
GlEnable                = validate_proc(opengl32,"glEnable",{C_UINT}),
GlEnableClientState     = validate_proc(opengl32,"glEnableClientState", {C_UINT}),
GlEnd                   = validate_proc(opengl32,"glEnd",{}),
GlEndList               = validate_proc(opengl32,"glEndList",{}),
--GlEvalMesh2           = validate_proc(opengl32,"glEvalMesh2",repeat(C_INT,5)),
GlFinish                = validate_proc(opengl32,"glFinish",{}),
GlFlush                 = validate_proc(opengl32,"glFlush",{}),
GlFogf                  = validate_proc(opengl32,"glFogf",{GLenum,GLfloat}),
GlFogfv                 = validate_proc(opengl32,"glFogfv",{C_INT,C_POINTER}),
GlFogi                  = validate_proc(opengl32,"glFogi",{C_INT,C_INT}),
--GlFogiv               = validate_proc(opengl32,"glFogiv",{C_INT,C_POINTER}),
GlFrontFace             = validate_proc(opengl32,"glFrontFace",{C_INT}),
GlFrustum               = validate_proc(opengl32,"glFrustum",repeat(C_DOUBLE,6)),
GlGenLists              = validate_func(opengl32,"glGenLists",{C_INT},C_INT),
GlGenTextures           = validate_proc(opengl32,"glGenTextures",{C_INT,C_POINTER}),
GlGetError              = validate_func(opengl32,"glGetError",{},C_INT),
GlGetBooleanv           = validate_proc(opengl32,"glGetBooleanv", {C_UINT, C_POINTER}),
GlGetDoublev            = validate_proc(opengl32,"glGetDoublev", {C_UINT, C_POINTER}),
GlGetFloatv             = validate_proc(opengl32,"glGetFloatv", {C_UINT, C_POINTER}),
GlGetIntegerv           = validate_proc(opengl32,"glGetIntegerv", {C_UINT, C_POINTER}),
GlGetString             = validate_func(opengl32,"glGetString", {C_UINT}, C_POINTER),
GlHint                  = validate_proc(opengl32,"glHint",{C_INT,C_INT}),
GlIndexi                = validate_proc(opengl32,"glIndexi",{C_INT}),
GlLightf                = validate_proc(opengl32,"glLightf",{GLenum,GLenum,GLfloat}),
GlLightfv               = validate_proc(opengl32,"glLightfv",{C_INT,C_INT,C_POINTER}),
GlLightModelf           = validate_proc(opengl32,"glLightModelf",{C_INT,C_FLOAT}),
GlLightModelfv          = validate_proc(opengl32,"glLightModelfv",{C_INT,C_POINTER}),
GlListBase              = validate_proc(opengl32,"glListBase",{C_UINT}),
GlLoadIdentity          = validate_proc(opengl32,"glLoadIdentity",{}),
GlLoadMatrixd           = validate_proc(opengl32,"glLoadMatrixd",{C_PTR}),
--GlMap2f               = validate_proc(opengl32,"glMap2f",{GLenum,GLfloat,GLfloat,GLint,GLint,GLfloat,GLfloat,GLint,GLint,C_POINTER}),
--GlMapGrid2f           = validate_proc(opengl32,"glMapGrid2f",{C_INT,C_FLOAT,C_FLOAT,C_INT,C_FLOAT,C_INT}),
GlMaterialf             = validate_proc(opengl32,"glMaterialf",{C_UINT,C_UINT,C_FLOAT}),
GlMaterialfv            = validate_proc(opengl32,"glMaterialfv",{C_INT,C_INT,C_POINTER}),
--GlMateriali           = validate_proc(opengl32,"glMateriali",{C_UINT,C_UINT,C_INT}),
GlMatrixMode            = validate_proc(opengl32,"glMatrixMode",{C_UINT}),
GlNewList               = validate_proc(opengl32,"glNewList",{C_UINT,C_INT}),
--GlNormal3b            = validate_proc(opengl32,"glNormal3b",{C_CHAR,C_CHAR,C_CHAR}),
GlNormal3d              = validate_proc(opengl32,"glNormal3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlNormal3f            = validate_proc(opengl32,"glNormal3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--GlNormal3i            = validate_proc(opengl32,"glNormal3i",{C_INT,C_INT,C_INT}),
--GlNormal3s            = validate_proc(opengl32,"glNormal3s",{C_SHORT,C_SHORT,C_SHORT}),
GlOrtho                 = validate_proc(opengl32,"glOrtho",repeat(C_DOUBLE,6)),
GlPixelStorei           = validate_proc(opengl32,"glPixelStorei",{C_INT,C_INT}),
GlPointSize             = validate_proc(opengl32,"glPointSize",{C_FLOAT}),
GlPolygonMode           = validate_proc(opengl32,"glPolygonMode",{C_INT,C_INT}),
GlPopMatrix             = validate_proc(opengl32,"glPopMatrix",{}),
GlPushMatrix            = validate_proc(opengl32,"glPushMatrix",{}),
GlRasterPos2i           = validate_proc(opengl32,"glRasterPos2i",{C_INT,C_INT}),
--GlReadBuffer          = validate_proc(opengl32,"glReadBuffer",{C_INT}),
--GlReadPixels          = validate_proc(opengl32,"glReadPixels",repeat(C_INT,6) & C_POINTER),
--GlRectd               = validate_proc(opengl32,"glRectd",repeat(C_DOUBLE,4)),
--GlRectdv              = validate_proc(opengl32,"glRectdv",{C_POINTER}),
GlRectf                 = validate_proc(opengl32,"glRectf",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--GlRectfv              = validate_proc(opengl32,"glRectfv",{C_POINTER}),
--GlRecti               = validate_proc(opengl32,"glRecti",repeat(C_INT,4)),
--GlRectiv              = validate_proc(opengl32,"glRectiv",{C_POINTER}),
--GlRects               = validate_proc(opengl32,"glRects",repeat(C_SHORT,4)),
--GlRectsv              = validate_proc(opengl32,"glRectsv",{C_POINTER}),
--GlRenderMode          = validate_func(opengl32,"glRenderMode",{C_INT},C_INT),
GlRotated               = validate_proc(opengl32,"glRotated",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
GlRotatef               = validate_proc(opengl32,"glRotatef",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
GlScaled                = validate_proc(opengl32,"glScaled",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
GlScalef                = validate_proc(opengl32,"glScalef",{C_FLOAT,C_FLOAT,C_FLOAT}),
GlScissor               = validate_proc(opengl32,"glScissor",repeat(C_INT,4)),
--GlSelectBuffer            = validate_proc(opengl32,"glSelectBuffer",{C_INT,C_POINTER}),
GlShadeModel            = validate_proc(opengl32,"glShadeModel",{C_UINT}),
--GlStencilFunc         = validate_proc(opengl32,"glStencilFunc",{C_INT,C_INT,C_UINT}),
--GlStencilMask         = validate_proc(opengl32,"glStencilMask",{C_UINT}),
--GlStencilOp           = validate_proc(opengl32,"glStencilOp",{C_INT,C_INT,C_INT}),
--GlTexCoord1d          = validate_proc(opengl32,"glTexCoord1d",{C_DOUBLE}),
--GlTexCoord1dv         = validate_proc(opengl32,"glTexCoord1dv",{C_POINTER}),
--GlTexCoord1fv         = validate_proc(opengl32,"glTexCoord1fv",{C_POINTER}),
--GlTexCoord1i          = validate_proc(opengl32,"glTexCoord1i",{C_INT}),
--GlTexCoord1iv         = validate_proc(opengl32,"glTexCoord1iv",{C_POINTER}),
--GlTexCoord1s          = validate_proc(opengl32,"glTexCoord1s",{C_SHORT}),
--GlTexCoord1sv         = validate_proc(opengl32,"glTexCoord1sv",{C_POINTER}),
--GlTexCoord2d          = validate_proc(opengl32,"glTexCoord2d",{C_DOUBLE,C_DOUBLE}),
--GlTexCoord2dv         = validate_proc(opengl32,"glTexCoord2dv",{C_POINTER}),
--GlTexCoord2f          = validate_proc(opengl32,"glTexCoord2f",{C_FLOAT,C_FLOAT}),
--GlTexCoord2fv         = validate_proc(opengl32,"glTexCoord2fv",{C_POINTER}),
--GlTexCoord2i          = validate_proc(opengl32,"glTexCoord2i",{C_INT,C_INT}),
--GlTexCoord2iv         = validate_proc(opengl32,"glTexCoord2iv",{C_POINTER}),
--GlTexCoord2s          = validate_proc(opengl32,"glTexCoord2s",{C_SHORT,C_SHORT}),
--GlTexCoord2sv         = validate_proc(opengl32,"glTexCoord2sv",{C_POINTER}),
--GlTexCoord3d          = validate_proc(opengl32,"glTexCoord3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlTexCoord3dv         = validate_proc(opengl32,"glTexCoord3dv",{C_POINTER}),
--GlTexCoord3f          = validate_proc(opengl32,"glTexCoord3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--GlTexCoord3fv         = validate_proc(opengl32,"glTexCoord3fv",{C_POINTER}),
--GlTexCoord3i          = validate_proc(opengl32,"glTexCoord3i",{C_INT,C_INT,C_INT}),
--GlTexCoord3iv         = validate_proc(opengl32,"glTexCoord3iv",{C_POINTER}),
--GlTexCoord3s          = validate_proc(opengl32,"glTexCoord3s",{C_SHORT,C_SHORT,C_SHORT}),
--GlTexCoord3sv         = validate_proc(opengl32,"glTexCoord3sv",{C_POINTER}),
GlTexCoord4d            = validate_proc(opengl32,"glTexCoord4d",repeat(C_DOUBLE,4)),
--GlTexCoord4dv         = validate_proc(opengl32,"glTexCoord4dv",{C_POINTER}),
--GlTexCoord4fv         = validate_proc(opengl32,"glTexCoord4fv",{C_POINTER}),
--GlTexCoord4i          = validate_proc(opengl32,"glTexCoord4i",repeat(C_INT,4)),
--GlTexCoord4iv         = validate_proc(opengl32,"glTexCoord4iv",{C_POINTER}),
--GlTexCoord4s          = validate_proc(opengl32,"glTexCoord4s",repeat(C_SHORT,4)),
--GlTexCoord4sv         = validate_proc(opengl32,"glTexCoord4sv",{C_POINTER}),
GlTexEnvi               = validate_proc(opengl32,"glTexEnvi",{C_INT,C_INT,C_INT}),
GlTexEnvf               = validate_proc(opengl32,"glTexEnvf",{C_INT,C_INT,C_FLOAT}),
--GlTexEnvf             = validate_proc(opengl32,"glTexEnvf",{C_INT,C_INT,C_DOUBLE}),
GlTexGeni               = validate_proc(opengl32,"glTexGeni",{C_INT,C_INT,C_INT}),
--GlTexImage1D          = validate_proc(opengl32,"glTexImage1D",repeat(C_INT,7) & C_POINTER),
GlTexImage2D            = validate_proc(opengl32,"glTexImage2D",repeat(C_INT,8) & C_POINTER),
--GlTexParameterfv      = validate_proc(opengl32,"glTexParameterfv",{C_INT,C_INT,C_POINTER}),
GlTexParameteri         = validate_proc(opengl32,"glTexParameteri",{C_INT,C_INT,C_INT}),
--GlTexParameteriv      = validate_proc(opengl32,"glTexParameteriv",{C_INT,C_INT,C_POINTER}),
GlTranslated            = validate_proc(opengl32,"glTranslated",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
GlTranslatef            = validate_proc(opengl32,"glTranslatef",{C_FLOAT,C_FLOAT,C_FLOAT}),
--glUniform
--GlVertex2d            = validate_proc(opengl32,"glVertex2d",{C_DOUBLE,C_DOUBLE}),
--GlVertex2dv           = validate_proc(opengl32,"glVertex2dv",{C_POINTER}),
--GlVertex2f            = validate_proc(opengl32,"glVertex2f",{C_FLOAT,C_FLOAT}),
--GlVertex2fv           = validate_proc(opengl32,"glVertex2fv",{C_POINTER}),
--GlVertex2i            = validate_proc(opengl32,"glVertex2i",{C_INT,C_INT}),
--GlVertex2iv           = validate_proc(opengl32,"glVertex2iv",{C_POINTER}),
--GlVertex2s            = validate_proc(opengl32,"glVertex2s",{C_SHORT,C_SHORT}),
--GlVertex2sv           = validate_proc(opengl32,"glVertex2sv",{C_POINTER}),
GlVertex3d              = validate_proc(opengl32,"glVertex3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlVertex3dv           = validate_proc(opengl32,"glVertex3dv",{C_POINTER}),
--GlVertex3f            = validate_proc(opengl32,"glVertex3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--GlVertex3fv           = validate_proc(opengl32,"glVertex3fv",{C_POINTER}),
--GlVertex3i            = validate_proc(opengl32,"glVertex3i",{C_INT,C_INT,C_INT}),
--GlVertex3iv           = validate_proc(opengl32,"glVertex3iv",{C_POINTER}),
--GlVertex3s            = validate_proc(opengl32,"glVertex3s",{C_SHORT,C_SHORT,C_SHORT}),
--GlVertex3sv           = validate_proc(opengl32,"glVertex3sv",{C_POINTER}),
GlVertex4d              = validate_proc(opengl32,"glVertex4d",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--GlVertex4dv           = validate_proc(opengl32,"glVertex4dv",{C_POINTER}),
--GlVertex4fv           = validate_proc(opengl32,"glVertex4fv",{C_POINTER}),
--GlVertex4f            = validate_proc(opengl32,"glVertex4f",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--GlVertex4i            = validate_proc(opengl32,"glVertex4i",{C_INT,C_INT,C_INT,C_INT}),
--GlVertex4iv           = validate_proc(opengl32,"glVertex4iv",{C_POINTER}),
--GlVertex4s            = validate_proc(opengl32,"glVertex4s",{C_SHORT,C_SHORT,C_SHORT,C_SHORT}),
--GlVertex4sv           = validate_proc(opengl32,"glVertex4sv",{C_POINTER}),
GlVertexPointer         = validate_proc(opengl32,"glVertexPointer", {C_INT, C_UINT, C_UINT, C_POINTER}),
GlViewport              = validate_proc(opengl32,"glViewport",{C_INT,C_INT,C_INT,C_INT}),
--WglGetProcAddress     = validate_func(opengl32,"wglGetProcAddress", {C_POINTER}, C_POINTER),
sglGetProcAddress       = iff(platform()=WINDOWS?"wglGetProcAddress":"glXGetProcAddress"),
XglGetProcAddress       = validate_func(opengl32,sglGetProcAddress, {C_POINTER}, C_POINTER)
--WglUseFontOutlines        = validate_func(opengl32,"wglUseFontOutlinesA",{C_UINT,C_INT,C_INT,C_INT,C_FLOAT,C_FLOAT,C_INT,C_POINTER},C_INT)

--global function xglGetProcAddress(string name)    --DEV rename as this/docs:
global function wglGetProcAddress(string name)
atom addr = c_func(XglGetProcAddress,{name})
    if addr<=0 then
        IupMessage("Error", "Couldn't find " & name)
        abort(1)
    end if
    return addr
end function

--1/12/16 (WglUseFontOutlines is Windows-only)
atom WglUseFontOutlines = NULL
global function wglUseFontOutlines(atom glhDC, integer first, integer count, atom pFontList, atom deviation, atom extrusion, integer fmt, atom pGMF)
    if WglUseFontOutlines=NULL then
        if platform()!=WINDOWS then ?9/0 end if
        WglUseFontOutlines = validate_func(opengl32,"wglUseFontOutlinesA",{C_UINT,C_INT,C_INT,C_INT,C_FLOAT,C_FLOAT,C_INT,C_POINTER},C_INT)
    end if
    return c_func(WglUseFontOutlines,{glhDC,first,count,pFontList,deviation,extrusion,fmt,pGMF})
end function

--/*
function link_glext_func(string name, sequence args, atom result)
atom addr = c_func(XglGetProcAddress,{name})
    return define_c_func({},addr,args,result)
end function
--*/

function link_glext_proc(string name, sequence args)
atom addr = c_func(XglGetProcAddress,{name})
    return define_c_proc({},addr,args)
end function

constant gl_vector_buffer = allocate(128)

procedure gl_pokef32(atom dest,sequence data)
    for i=1 to length(data) do
        poke(dest,atom_to_float32(data[i]))
        dest += 4
    end for
end procedure

--/* (not [yet] used):
procedure gl_pokef64(atom dest,sequence data)
    for i=1 to length(data) do
        poke(dest,atom_to_float64(data[i]))
        dest += 8
    end for
end procedure
--*/

global function glGetError()
    return c_func(GlGetError,{})
end function

global procedure glAccum(integer op, atom val)
    c_proc(GlAccum,{op,val})
end procedure

global procedure glBegin(integer what)
    c_proc(GlBegin,{what})
end procedure

global procedure glBindTexture(integer target, atom texture)
    c_proc(GlBindTexture,{target,texture})
end procedure

global procedure glBitmap(integer width, height, atom xorig, yorig, xmove, ymove, bitmap)
    c_proc(GlBitmap,{width,height,xorig,yorig,xmove,ymove,bitmap})
end procedure

global procedure glBlendFunc(integer sfactor, dfactor)
    c_proc(GlBlendFunc,{sfactor,dfactor})
end procedure

global procedure glCallList(integer list)
    c_proc(GlCallList,{list})
end procedure

global procedure glCallLists(integer n, integer typ, atom lists)
    c_proc(GlCallLists,{n,typ,lists})
end procedure

global procedure glClear(integer mask)
    c_proc(GlClear,{mask})
end procedure

global procedure glClearAccum(atom r, g, b, a)
    c_proc(GlClearAccum,{r,g,b,a})
end procedure

global procedure glClearColor(atom r, g, b, a)
    c_proc(GlClearColor,{r,g,b,a})
end procedure

global procedure glClearDepth(atom depth)
    c_proc(GlClearDepth,{depth})
end procedure

global procedure glColor(atom red, blue, green, alpha=1)
    c_proc(GlColor4d,{red,blue,green,alpha})
end procedure

--global procedure glColor3d(sequence col)
global procedure glColor3(sequence colour)
    if length(colour)=3 then
        c_proc(GlColor3d,colour)
    else
        c_proc(GlColor4d,colour)
    end if
end procedure

--global procedure glColor3f(sequence col)
--  c_proc(GlColor3f,col)
--end procedure

--global procedure glColor4f(sequence col)
--  c_proc(GlColor4f,col)
--end procedure

global procedure glColorPointer(integer size, integer ptype, integer stride, atom pointer)
    c_proc(GlColorPointer, {size, ptype, stride, pointer})
end procedure

global procedure glCullFace(integer mode)
    c_proc(GlCullFace,{mode})
end procedure

global procedure glDeleteLists(atom list,integer range)
    c_proc(GlDeleteLists,{list,range})
end procedure

global procedure glDepthFunc(integer f)
    c_proc(GlDepthFunc,{f})
end procedure

global procedure glDepthMask(integer flag)
    c_proc(GlDepthMask, {flag})
end procedure

global procedure glDisable(integer what)
    c_proc(GlDisable,{what})
end procedure

global procedure glDisableClientState(integer cap)
    c_proc(GlDisableClientState, {cap})
end procedure

global procedure glDrawElements(integer mode, integer count, integer stype, atom indices)
    c_proc(GlDrawElements, {mode, count, stype, indices})
end procedure

global procedure glEnable(integer what)
    c_proc(GlEnable,{what})
end procedure

global procedure glEnableClientState(integer cap)
    c_proc(GlEnableClientState, {cap})
end procedure

global procedure glEnd()
    c_proc(GlEnd,{})
end procedure

global procedure glEndList()
    c_proc(GlEndList,{})
end procedure

global procedure glFinish()
    c_proc(GlFinish,{})
end procedure

global procedure glFlush()
    c_proc(GlFlush,{})
end procedure

global procedure glFogf(integer pname, atom param)
    c_proc(GlFogf,{pname,param})
end procedure

global procedure glFogfv(integer pname, sequence params)
    gl_pokef32(gl_vector_buffer,params)
    c_proc(GlFogfv,{pname,gl_vector_buffer})
end procedure

global procedure glFogi(integer pname, integer param)
    c_proc(GlFogi,{pname,param})
end procedure

global procedure glFrontFace(integer mode)
    c_proc(GlFrontFace,{mode})
end procedure

global procedure glFrustum(atom left, right, top, bottom, zNear, zFar)
    c_proc(GlFrustum,{left,right,top,bottom,zNear,zFar})
end procedure

global function glGenLists(integer range)
    return c_func(GlGenLists,{range})
end function

global function glGenTextures(integer n=1)
    atom pMem = allocate(4*n)
    c_proc(GlGenTextures,{n,pMem})
    integer e = glGetError()
    if e!=GL_NO_ERROR then ?9/0 end if
    sequence textures = peek4u({pMem,n})
    free(pMem)
    return textures
end function

global procedure glGetBooleanv(integer pname, atom pMem)
    c_proc(GlGetBooleanv,{pname,pMem})
end procedure

global procedure glGetDoublev(integer pname, atom pMem)
    c_proc(GlGetDoublev,{pname,pMem})
end procedure

global procedure glGetFloatv(integer pname, atom pMem)
    c_proc(GlGetFloatv,{pname,pMem})
end procedure

global procedure glGetIntegerv(integer pname, atom pMem)
    c_proc(GlGetIntegerv,{pname,pMem})
end procedure

global function glGetString(integer name)
integer len
atom ret
    ret = c_func(GlGetString, {name})
    if ret then
--DEV return peek_string(ret)
        len = 0
        while peek(ret+len) do len += 1 end while
        return peek({ret, len})
    else
        return ""
    end if
end function

object supported_extensions = NULL
global function glSupportsExtension(sequence ext)
    if supported_extensions=NULL then
        supported_extensions = split(glGetString(GL_EXTENSIONS))
    end if
    return find(ext,supported_extensions)!=0
end function

atom GlFogCoordfEXT,
     GlFogCoordfvEXT,
     GlFogCoorddEXT
--   GlFogCoorddvEXT

global procedure enable_GL_EXT_fog_coord()
    GlFogCoordfEXT = link_glext_proc("glFogCoordfEXT",{GLfloat})
    GlFogCoordfvEXT = link_glext_proc("glFogCoordfvEXT",{C_POINTER})
    GlFogCoorddEXT = link_glext_proc("glFogCoorddEXT",{GLdouble})
--  GlFogCoorddvEXT = link_glext_proc("glFogCoorddvEXT",{C_POINTER})
end procedure

global procedure glFogCoordfEXT(atom a)
--?9/0
--  poke4(call_by_ptr__num_params,1)
--  poke(call_by_ptr_params,atom_to_float32(a))
--  poke4(call_by_ptr__func_addr,GlFogCoordfEXT)
--  call(call_by_ptr_code)
    c_proc(GlFogCoordfEXT,{a})
end procedure

global procedure glFogCoordfvEXT(sequence /*a*/)
?9/0
--  gl_pokef32(gl_vector_buffer,a)
--  poke4(call_by_ptr__num_params,1)
--  poke4(call_by_ptr_params,gl_vector_buffer)
--  poke4(call_by_ptr__func_addr,glFogCoordfvEXT)
--  call(call_by_ptr_code)
--  c_proc(glFogCoordfvEXT,{gl_vector_buffer})
end procedure

global procedure glFogCoorddEXT(atom /*a*/)
?9/0
--  sequence f64
--  poke4(call_by_ptr__num_params,2)
--  f64 = atom_to_float64(a)
--  poke(call_by_ptr_params,f64[5..8])
--  poke(call_by_ptr_params+4,f64[1..4])
--  poke4(call_by_ptr__func_addr,GlFogCoorddEXT)
--  call(call_by_ptr_code)
--  c_proc(GlFogCoorddEXT,{??})
end procedure


global procedure glHint(integer target,integer mode)
    c_proc(GlHint,{target,mode})
end procedure

global procedure glIndexi(integer idx)
    c_proc(GlIndexi,{idx})
end procedure

global procedure glLight(integer light, integer pname, object params)
    if atom(params) then
        c_proc(GlLightf,{light,pname,params})
    else
        gl_pokef32(gl_vector_buffer,params)
        c_proc(GlLightfv,{light,pname,gl_vector_buffer})
    end if
end procedure

--global procedure glLightf(integer light,integer pname,atom val)
--  c_proc(GlLightf,{light,pname,val})
--end procedure
--
--global procedure glLightfv(integer light,integer pname,sequence vals)
--  gl_pokef32(gl_vector_buffer,vals)
--  c_proc(GlLightfv,{light,pname,gl_vector_buffer})
--end procedure

global procedure glLightModelf(integer i,atom val)
    c_proc(GlLightModelf,{i,val})
end procedure

global procedure glLightModelfv(integer i,sequence vals)
    gl_pokef32(gl_vector_buffer,vals)
    c_proc(GlLightModelfv,{i,gl_vector_buffer})
end procedure

global procedure glListBase(atom base)
    c_proc(GlListBase,{base})
end procedure

global procedure glLoadIdentity()
    c_proc(GlLoadIdentity,{})
end procedure

global procedure glLoadMatrixd(atom m)
    c_proc(GlLoadMatrixd,{m})
end procedure

global procedure glMaterialf(integer face,integer pname,atom val)
    c_proc(GlMaterialf,{face,pname,val})
end procedure

global procedure glMaterialfv(integer face,integer pname,sequence vals)
    gl_pokef32(gl_vector_buffer,vals)
    c_proc(GlMaterialfv,{face,pname,gl_vector_buffer})
end procedure

global procedure glMatrixMode(integer mode)
    c_proc(GlMatrixMode,{mode})
end procedure

global procedure glNewList(integer list,integer mode)
    c_proc(GlNewList,{list,mode})
end procedure

global procedure glNormal(atom nx, atom ny, atom nz)
    c_proc(GlNormal3d,{nx,ny,nz})
end procedure

global procedure glNormal3(sequence s)
    c_proc(GlNormal3d,s)
end procedure

--global procedure glNormal3b(sequence normal)
--  c_proc(GlNormal3b,normal)
--end procedure

--global procedure glNormal3d(atom nx, atom ny, atom nz)
--  c_proc(GlNormal3d,{nx,ny,nz})
--end procedure

--global procedure glNormal3f(atom nx, atom ny, atom nz)
--  c_proc(GlNormal3f,{nx,ny,nz})
--end procedure

--global procedure glNormal3i(sequence normal)
--  c_proc(GlNormal3i,normal)
--end procedure
--
--global procedure glNormal3s(sequence normal)
--  c_proc(GlNormal3s,normal)
--end procedure

global procedure glOrtho(atom left, right, top, bottom, zNear, zFar)
    c_proc(GlOrtho,{left,right,top,bottom,zNear,zFar})
end procedure

global procedure glPixelStorei(integer pname,integer param)
    c_proc(GlPixelStorei,{pname,param})
end procedure

global procedure glPointSize(atom size)
    c_proc(GlPointSize,{size})
end procedure

global procedure glPolygonMode(integer face,integer mode)
    c_proc(GlPolygonMode,{face,mode})
end procedure

global procedure glPopMatrix()
    c_proc(GlPopMatrix,{})
end procedure

global procedure glPushMatrix()
    c_proc(GlPushMatrix,{})
end procedure

global procedure glRasterPos2i(sequence pos)
    c_proc(GlRasterPos2i,pos)
end procedure

global procedure glRectf(sequence rect)
    c_proc(GlRectf,rect)
end procedure

global procedure glRotate(atom angle, x, y, z)
    c_proc(GlRotated,{angle,x,y,z})
end procedure

global procedure glRotated(atom angle, x, y, z)
    c_proc(GlRotated,{angle,x,y,z})
end procedure

global procedure glRotatef(atom angle, x, y, z)
    c_proc(GlRotatef,{angle,x,y,z})
end procedure

global procedure glScale(sequence scale)
    c_proc(GlScaled,scale)
end procedure

global procedure glScaled(sequence scale)
    c_proc(GlScaled,scale)
end procedure

global procedure glScalef(sequence scale)
    c_proc(GlScalef,scale)
end procedure

global procedure glScissor(integer x, y, width, height)
    c_proc(GlScissor, {x, y, width, height})
end procedure

global procedure glShadeModel(integer model)
    c_proc(GlShadeModel,{model})
end procedure

global procedure glTexCoord(atom s, t=0, r=0, q=1)
    c_proc(GlTexCoord4d,{s,t,r,q})
end procedure

global procedure glTexImage2D(integer target, level, components, width, height, border, fmt, typ, atom pixels)
    c_proc(GlTexImage2D,{target,level,components,width,height,border,fmt,typ,pixels})
end procedure

global procedure glTexParameteri(integer target, pname, param)
    c_proc(GlTexParameteri,{target,pname,param})
end procedure

global procedure glTexEnvi(integer i, j, k)
    c_proc(GlTexEnvi,{i,j,k})
end procedure

global procedure glTexEnvf(integer i, j, atom k)
    c_proc(GlTexEnvf,{i,j,k})
end procedure

global procedure glTexGeni(integer coord, pname, param)
    c_proc(GlTexGeni,{coord,pname,param})
end procedure

global procedure glTranslate(atom x, y, z)
    c_proc(GlTranslated,{x,y,z})
end procedure

global procedure glTranslated(atom x, y, z)
    c_proc(GlTranslated,{x,y,z})
end procedure

global procedure glTranslatef(atom x, y, z)
    c_proc(GlTranslatef,{x,y,z})
end procedure

global procedure glVertex(atom x, y, z=0, w=1)
    c_proc(GlVertex4d,{x,y,z,w})
end procedure

--/*
global procedure glVertex2d(sequence vertex)
    c_proc(GlVertex2d,vertex)
end procedure

global procedure glVertex2f(sequence vertex)
    c_proc(GlVertex2f,vertex)
end procedure

global procedure glVertex2i(sequence vertex)
    c_proc(GlVertex2i,vertex)
end procedure

global procedure glVertex2s(sequence vertex)
    c_proc(GlVertex2s,vertex)
end procedure
--*/

global procedure glVertex3d(sequence vertex)
    c_proc(GlVertex3d,vertex)
end procedure

--/*
global procedure glVertex3f(sequence vertex)
    c_proc(GlVertex3f,vertex)
end procedure

global procedure glVertex3i(sequence vertex)
    c_proc(GlVertex3i,vertex)
end procedure

global procedure glVertex3s(sequence vertex)
    c_proc(GlVertex3s,vertex)
end procedure

global procedure glVertex4d(sequence vertex)
    c_proc(GlVertex4d,vertex)
end procedure

global procedure glVertex4f(sequence vertex)
    c_proc(GlVertex4f,vertex)
end procedure

global procedure glVertex4i(sequence vertex)
    c_proc(GlVertex4i,vertex)
end procedure

global procedure glVertex4s(sequence vertex)
    c_proc(GlVertex4s,vertex)
end procedure
--*/

--global procedure glVertex3dv(sequence vertex)
--  c_proc(GlVertex3d,vertex)
--end procedure

global procedure glVertexPointer(integer size, ptype, stride, atom pointer)
    c_proc(GlVertexPointer, {size, ptype, stride, pointer})
end procedure

global procedure glViewport(integer x, y, w, h)
    c_proc(GlViewport,{x,y,w,h})
end procedure

--/*
GlmfBeginGlsBlock
GlmfCloseMetaFile
GlmfEndGlsBlock
GlmfEndPlayback
GlmfInitPlayback
GlmfPlayGlsRecord
glAccum
glAlphaFunc
glAreTexturesResident
glArrayElement
glBegin
glBindTexture
glBitmap
glBlendFunc
glCallList
glCallLists
glClear
glClearAccum
glClearColor
glClearDepth
glClearIndex
glClearStencil
glClipPlane
glColor3b
glColor3bv
glColor3d
glColor3dv
glColor3f
glColor3fv
glColor3i
glColor3iv
glColor3s
glColor3sv
glColor3ub
glColor3ubv
glColor3ui
glColor3uiv
glColor3us
glColor3usv
glColor4b
glColor4bv
glColor4d
glColor4dv
glColor4f
glColor4fv
glColor4i
glColor4iv
glColor4s
glColor4sv
glColor4ub
glColor4ubv
glColor4ui
glColor4uiv
glColor4us
glColor4usv
glColorMask
glColorMaterial
glColorPointer
glCopyPixels
glCopyTexImage1D
glCopyTexImage2D
glCopyTexSubImage1D
glCopyTexSubImage2D
glCullFace
glDebugEntry
glDeleteLists
glDeleteTextures
glDepthFunc
glDepthMask
glDepthRange
glDisable
glDisableClientState
glDrawArrays
glDrawBuffer
glDrawElements
glDrawPixels
glEdgeFlag
glEdgeFlagPointer
glEdgeFlagv
glEnable
glEnableClientState
glEnd
glEndList
glEvalCoord1d
glEvalCoord1dv
glEvalCoord1f
glEvalCoord1fv
glEvalCoord2d
glEvalCoord2dv
glEvalCoord2f
glEvalCoord2fv
glEvalMesh1
glEvalMesh2
glEvalPoint1
glEvalPoint2
glFeedbackBuffer
glFinish
glFlush
glFogf
glFogfv
glFogi
glFogiv
glFrontFace
glFrustum
glGenLists
glGenTextures
glGetBooleanv
glGetClipPlane
glGetDoublev
glGetError
glGetFloatv
glGetIntegerv
glGetLightfv
glGetLightiv
glGetMapdv
glGetMapfv
glGetMapiv
glGetMaterialfv
glGetMaterialiv
glGetPixelMapfv
glGetPixelMapuiv
glGetPixelMapusv
glGetPointerv
glGetPolygonStipple
glGetString
glGetTexEnvfv
glGetTexEnviv
glGetTexGendv
glGetTexGenfv
glGetTexGeniv
glGetTexImage
glGetTexLevelParameterfv
glGetTexLevelParameteriv
glGetTexParameterfv
glGetTexParameteriv
glHint
glIndexMask
glIndexPointer
glIndexd
glIndexdv
glIndexf
glIndexfv
glIndexi
glIndexiv
glIndexs
glIndexsv
glIndexub
glIndexubv
glInitNames
glInterleavedArrays
glIsEnabled
glIsList
glIsTexture
glLightModelf
glLightModelfv
glLightModeli
glLightModeliv
glLightf
glLightfv
glLighti
glLightiv
glLineStipple
glLineWidth
glListBase
glLoadIdentity
glLoadMatrixd
glLoadMatrixf
glLoadName
glLogicOp
glMap1d
glMap1f
glMap2d
glMap2f
glMapGrid1d
glMapGrid1f
glMapGrid2d
glMapGrid2f
glMaterialf
glMaterialfv
glMateriali
glMaterialiv
glMatrixMode
glMultMatrixd
glMultMatrixf
glNewList
glNormal3b
glNormal3bv
glNormal3d
glNormal3dv
glNormal3f
glNormal3fv
glNormal3i
glNormal3iv
glNormal3s
glNormal3sv
glNormalPointer
glOrtho
glPassThrough
glPixelMapfv
glPixelMapuiv
glPixelMapusv
glPixelStoref
glPixelStorei
glPixelTransferf
glPixelTransferi
glPixelZoom
glPointSize
glPolygonMode
glPolygonOffset
glPolygonStipple
glPopAttrib
glPopClientAttrib
glPopMatrix
glPopName
glPrioritizeTextures
glPushAttrib
glPushClientAttrib
glPushMatrix
glPushName
glRasterPos2d
glRasterPos2dv
glRasterPos2f
glRasterPos2fv
glRasterPos2i
glRasterPos2iv
glRasterPos2s
glRasterPos2sv
glRasterPos3d
glRasterPos3dv
glRasterPos3f
glRasterPos3fv
glRasterPos3i
glRasterPos3iv
glRasterPos3s
glRasterPos3sv
glRasterPos4d
glRasterPos4dv
glRasterPos4f
glRasterPos4fv
glRasterPos4i
glRasterPos4iv
glRasterPos4s
glRasterPos4sv
glReadBuffer
glReadPixels
glRectd
glRectdv
glRectf
glRectfv
glRecti
glRectiv
glRects
glRectsv
glRenderMode
glRotated
glRotatef
glScaled
glScalef
glScissor
glSelectBuffer
glShadeModel
glStencilFunc
glStencilMask
glStencilOp
glTexCoord1d
glTexCoord1dv
glTexCoord1f
glTexCoord1fv
glTexCoord1i
glTexCoord1iv
glTexCoord1s
glTexCoord1sv
glTexCoord2d
glTexCoord2dv
glTexCoord2f
glTexCoord2fv
glTexCoord2i
glTexCoord2iv
glTexCoord2s
glTexCoord2sv
glTexCoord3d
glTexCoord3dv
glTexCoord3f
glTexCoord3fv
glTexCoord3i
glTexCoord3iv
glTexCoord3s
glTexCoord3sv
glTexCoord4d
glTexCoord4dv
glTexCoord4f
glTexCoord4fv
glTexCoord4i
glTexCoord4iv
glTexCoord4s
glTexCoord4sv
glTexCoordPointer
glTexEnvf
glTexEnvfv
glTexEnvi
glTexEnviv
glTexGend
glTexGendv
glTexGenf
glTexGenfv
glTexGeni
glTexGeniv
glTexImage1D
glTexImage2D
glTexParameterf
glTexParameterfv
glTexParameteri
glTexParameteriv
glTexSubImage1D
glTexSubImage2D
glTranslated
glTranslatef
glVertex2d
glVertex2dv
glVertex2f
glVertex2fv
glVertex2i
glVertex2iv
glVertex2s
glVertex2sv
glVertex3d
glVertex3dv
glVertex3f
glVertex3fv
glVertex3i
glVertex3iv
glVertex3s
glVertex3sv
glVertex4d
glVertex4dv
glVertex4f
glVertex4fv
glVertex4i
glVertex4iv
glVertex4s
glVertex4sv
glVertexPointer
glViewport
wglChoosePixelFormat
wglCopyContext
wglCreateContext
wglCreateLayerContext
wglDeleteContext
wglDescribeLayerPlane
wglDescribePixelFormat
wglGetCurrentContext
wglGetCurrentDC
wglGetDefaultProcAddress
wglGetLayerPaletteEntries
wglGetPixelFormat
wglGetProcAddress
wglMakeCurrent
wglRealizeLayerPalette
wglSetLayerPaletteEntries
wglSetPixelFormat
wglShareLists
wglSwapBuffers
wglSwapLayerBuffers
wglSwapMultipleBuffers
wglUseFontBitmapsA
wglUseFontBitmapsW
wglUseFontOutlinesA
wglUseFontOutlinesW
--*/
