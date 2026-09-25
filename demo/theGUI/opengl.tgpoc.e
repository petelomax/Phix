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
--
-- WORK IN PROGRESS: unifying WebGL for pwa/p2js and Open GL ES 2.0 for desktop/Phix
--                   (by Pete Lomax, September 2021 onwards)
--

--/*
include std/os.e
include std/machine.e
include std/dll.e
include std/get.e
include std/convert.e
--*/
--include validate.e

--include pGUI.e
include glmath.e
include cffi.e
--DOH, this is not transpiled!
--IupGLMakeCurrent(NULL)    -- signal pGUI.js that opengl.e /has/ been included 
                        -- note that disables canvasdraw, or risks doing so
                        -- (the above is now a null-op on desktop/Phix btw)

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
--X     GL_VERTEX_ARRAY
--      GL_NORMAL_ARRAY
--      GL_COLOR_ARRAY
--      GL_INDEX_ARRAY
--      GL_TEXTURE_COORD_ARRAY
--      GL_EDGE_FLAG_ARRAY
--      GL_CULL_VERTEX_SGI
--      GL_INDEX_MATERIAL_SGI
--      GL_INDEX_TEST_SGI


--DEV doc
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
                GL_EDGE_FLAG_ARRAY_STRIDE           = #808C,

                GL_TEXTURE_0                        = #84C0,

                GL_INFO_LOG_LENGTH                  = #8B84,
                GL_SHADER_SOURCE_LENGTH             = #8B88,
                GL_COMPILE_STATUS                   = #8B81, 
                GL_LINK_STATUS                      = #8B82, 
                GL_ARRAY_BUFFER                     = #8892,
                GL_STATIC_DRAW                      = #88E4
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

--DEV use the routines in pGUI.e: (ie iup_c_proc, iup_c_func)
--global function iup_c_func(atom dll, sequence name, sequence args, atom result, boolean allow_fail=false)
--global function iup_c_proc(atom dll, sequence name, sequence args)
--global 
--function validate_proc(atom lib, sequence name, sequence parms)
--  integer hProc = define_c_proc(lib,name,parms)
--  if hProc = -1 then
--      crash("Error! Can\'t find "&name&"().")
--  end if
--  return hProc
--end function


--global 
--function validate_func(atom lib, sequence name, sequence parms, atom rtype)
--  integer hFunc = define_c_func(lib,name,parms,rtype)
--  if hFunc = -1 then
--      crash("Error! Can\'t find "&name&"().")
--  end if
--  return hFunc
--end function

global constant GL_LIBPATH = "/usr/lib/x86_64-linux-gnu/"

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
global constant GL_VERSION_1_1                  = 1,
                GL_EXT_abgr                     = 1,
                GL_EXT_bgra                     = 1,
                GL_EXT_packed_pixels            = 1,
                GL_EXT_paletted_texture         = 1,
                GL_EXT_vertex_array             = 1,
                GL_SGI_compiled_vertex_array    = 1,
                GL_SGI_cull_vertex              = 1,
                GL_SGI_index_array_formats      = 1,
                GL_SGI_index_func               = 1,
                GL_SGI_index_material           = 1,
                GL_SGI_index_texture            = 1,
                GL_WIN_swap_hint                = 1,

                -- ClientAttribMask
                GL_CLIENT_PIXEL_STORE_BIT       = #00000001,
                GL_CLIENT_VERTEX_ARRAY_BIT      = #00000002,
                GL_CLIENT_ALL_ATTRIB_BITS       = #FFFFFFFF,

                -- Boolean (I recommend /not/ using these, as in
                --          "why confuse/distract/worry yourself?")
                GL_FALSE = false,
                GL_TRUE  = true,

                -- AccumOp
                GL_ACCUM                        = #0100,
                GL_LOAD                         = #0101,
                GL_RETURN                       = #0102,
                GL_MULT                         = #0103,
                GL_ADD                          = #0104,

                -- AlphaFunction
                GL_NEVER                        = #0200,
                GL_LESS                         = #0201,
                GL_EQUAL                        = #0202,
                GL_LEQUAL                       = #0203,
                GL_GREATER                      = #0204,
                GL_NOTEQUAL                     = #0205,
                GL_GEQUAL                       = #0206,
                GL_ALWAYS                       = #0207,

                -- BlendingFactorDest
                GL_ZERO = 0,
                GL_ONE  = 1,
                GL_SRC_COLOR                    = #0300,
                GL_ONE_MINUS_SRC_COLOR          = #0301,
                GL_SRC_ALPHA                    = #0302,
                GL_ONE_MINUS_SRC_ALPHA          = #0303,
                GL_DST_ALPHA                    = #0304,
                GL_ONE_MINUS_DST_ALPHA          = #0305,

                -- BlendingFactorSrc
--      GL_ZERO
--      GL_ONE
                GL_DST_COLOR                    = #0306,
                GL_ONE_MINUS_DST_COLOR          = #0307,
                GL_SRC_ALPHA_SATURATE           = #0308,
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
                GL_NONE = 0,
                GL_FRONT_LEFT                   = #0400,
                GL_FRONT_RIGHT                  = #0401,
                GL_BACK_LEFT                    = #0402,
                GL_BACK_RIGHT                   = #0403,
                GL_FRONT                        = #0404,
                GL_BACK                         = #0405,
                GL_LEFT                         = #0406,
                GL_RIGHT                        = #0407,
                GL_FRONT_AND_BACK               = #0408,
                GL_AUX0                         = #0409,
                GL_AUX1                         = #040A,
                GL_AUX2                         = #040B,
                GL_AUX3                         = #040C,

                -- FeedbackType
                GL_2D                           = #0600,
                GL_3D                           = #0601,
                GL_3D_COLOR                     = #0602,
                GL_3D_COLOR_TEXTURE             = #0603,
                GL_4D_COLOR_TEXTURE             = #0604,

                -- FeedBackToken
                GL_PASS_THROUGH_TOKEN           = #0700,
                GL_POINT_TOKEN                  = #0701,
                GL_LINE_TOKEN                   = #0702,
                GL_POLYGON_TOKEN                = #0703,
                GL_BITMAP_TOKEN                 = #0704,
                GL_DRAW_PIXEL_TOKEN             = #0705,
                GL_COPY_PIXEL_TOKEN             = #0706,
                GL_LINE_RESET_TOKEN             = #0707,

                -- FogMode
--      GL_LINEAR
                GL_EXP                          = #0800,
                GL_EXP2                         = #0801,

                GL_FRAGMENT_SHADER              = #8B30,
                GL_VERTEX_SHADER                = #8B31,

                -- FogParameter
--      GL_FOG_COLOR
--      GL_FOG_DENSITY
--      GL_FOG_END
--      GL_FOG_INDEX
--      GL_FOG_MODE
--      GL_FOG_START

                -- FrontFaceDirection
                GL_CW                           = #0900,
                GL_CCW                          = #0901,

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
                GL_COEFF                        = #0A00,
                GL_ORDER                        = #0A01,
                GL_DOMAIN                       = #0A02,

                -- GetPixelMap
                GL_PIXEL_MAP_I_TO_I             = #0C70,
                GL_PIXEL_MAP_S_TO_S             = #0C71,
                GL_PIXEL_MAP_I_TO_R             = #0C72,
                GL_PIXEL_MAP_I_TO_G             = #0C73,
                GL_PIXEL_MAP_I_TO_B             = #0C74,
                GL_PIXEL_MAP_I_TO_A             = #0C75,
                GL_PIXEL_MAP_R_TO_R             = #0C76,
                GL_PIXEL_MAP_G_TO_G             = #0C77,
                GL_PIXEL_MAP_B_TO_B             = #0C78,
                GL_PIXEL_MAP_A_TO_A             = #0C79,

                -- GetPointervPName
                GL_VERTEX_ARRAY_POINTER         = #808E,
                GL_NORMAL_ARRAY_POINTER         = #808F,
                GL_COLOR_ARRAY_POINTER          = #8090,
                GL_INDEX_ARRAY_POINTER          = #8091,
                GL_TEXTURE_COORD_ARRAY_POINTER  = #8092,
                GL_EDGE_FLAG_ARRAY_POINTER      = #8093,

                -- GetTextureParameter
--      GL_TEXTURE_MAG_FILTER
--      GL_TEXTURE_MIN_FILTER
--      GL_TEXTURE_WRAP_S
--      GL_TEXTURE_WRAP_T
                GL_TEXTURE_WIDTH                = #1000,
                GL_TEXTURE_HEIGHT               = #1001,
                GL_TEXTURE_INTERNAL_FORMAT      = #1003,
                GL_TEXTURE_COMPONENTS           = GL_TEXTURE_INTERNAL_FORMAT,
                GL_TEXTURE_BORDER_COLOR         = #1004,
                GL_TEXTURE_BORDER               = #1005,
                GL_TEXTURE_RED_SIZE             = #805C,
                GL_TEXTURE_GREEN_SIZE           = #805D,
                GL_TEXTURE_BLUE_SIZE            = #805E,
                GL_TEXTURE_ALPHA_SIZE           = #805F,
                GL_TEXTURE_LUMINANCE_SIZE       = #8060,
                GL_TEXTURE_INTENSITY_SIZE       = #8061,
                GL_TEXTURE_PRIORITY             = #8066,
                GL_TEXTURE_RESIDENT             = #8067,

                -- HintMode
                GL_DONT_CARE                    = #1100,
                GL_FASTEST                      = #1101,
                GL_NICEST                       = #1102,

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
                GL_COMPILE                      = #1300,
                GL_COMPILE_AND_EXECUTE          = #1301,

                -- DataType
                GL_BYTE                         = #1400,
                GL_UNSIGNED_BYTE                = #1401,
                GL_SHORT                        = #1402,
                GL_UNSIGNED_SHORT               = #1403,
                GL_INT                          = #1404,
                GL_UNSIGNED_INT                 = #1405,
                GL_FLOAT                        = #1406,
                GL_2_BYTES                      = #1407,
                GL_3_BYTES                      = #1408,
                GL_4_BYTES                      = #1409,
                GL_DOUBLE                       = #140A,
                GL_DOUBLE_EXT                   = #140A,

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
                GL_CLEAR                        = #1500,
                GL_AND                          = #1501,
                GL_AND_REVERSE                  = #1502,
                GL_COPY                         = #1503,
                GL_AND_INVERTED                 = #1504,
                GL_NOOP                         = #1505,
                GL_XOR                          = #1506,
                GL_OR                           = #1507,
                GL_NOR                          = #1508,
                GL_EQUIV                        = #1509,
                GL_INVERT                       = #150A,
                GL_OR_REVERSE                   = #150B,
                GL_COPY_INVERTED                = #150C,
                GL_OR_INVERTED                  = #150D,
                GL_NAND                         = #150E,
                GL_SET                          = #150F,

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
                GL_EMISSION                     = #1600,
                GL_SHININESS                    = #1601,
                GL_AMBIENT_AND_DIFFUSE          = #1602,
                GL_COLOR_INDEXES                = #1603,
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
                GL_COLOR                        = #1800,
                GL_DEPTH                        = #1801,
                GL_STENCIL                      = #1802,
                
                -- PixelFormat
                GL_COLOR_INDEX                  = #1900,
                GL_STENCIL_INDEX                = #1901,
                GL_DEPTH_COMPONENT              = #1902,
                GL_RED                          = #1903,
                GL_GREEN                        = #1904,
                GL_BLUE                         = #1905,
                GL_ALPHA                        = #1906,
                GL_RGB                          = #1907,
                GL_RGBA                         = #1908,
                GL_LUMINANCE                    = #1909,
                GL_LUMINANCE_ALPHA              = #190A,
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
                GL_BITMAP                       = #1A00,
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
                GL_POINT                        = #1B00,
                GL_LINE                         = #1B01,
                GL_FILL                         = #1B02,

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
                GL_RENDER                       = #1C00,
                GL_FEEDBACK                     = #1C01,
                GL_SELECT                       = #1C02,

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
                GL_KEEP                         = #1E00,
                GL_REPLACE                      = #1E01,
                GL_INCR                         = #1E02,
                GL_DECR                         = #1E03,
--      GL_INVERT

                -- TexCoordPointerType
--      GL_SHORT
--      GL_INT
--      GL_FLOAT
--      GL_DOUBLE

                -- TextureCoordName
                GL_S                            = #2000,
                GL_T                            = #2001,
                GL_R                            = #2002,
                GL_Q                            = #2003,

                -- TextureEnvMode
                GL_MODULATE                     = #2100,
                GL_DECAL                        = #2101,
--      GL_BLEND
--      GL_REPLACE
--      GL_ADD

                -- TextureEnvParameter
                GL_TEXTURE_ENV_MODE             = #2200,
                GL_TEXTURE_ENV_COLOR            = #2201,

                -- TextureEnvTarget
                GL_TEXTURE_ENV                  = #2300,

                -- TextureGenMode
                GL_EYE_LINEAR                   = #2400,
                GL_OBJECT_LINEAR                = #2401,
                GL_SPHERE_MAP                   = #2402,

                -- TextureGenParameter
                GL_TEXTURE_GEN_MODE             = #2500,
                GL_OBJECT_PLANE                 = #2501,
                GL_EYE_PLANE                    = #2502,

                -- TextureMagFilter
                GL_NEAREST                      = #2600,
                GL_LINEAR                       = #2601,

                -- TextureMinFilter
--      GL_NEAREST
--      GL_LINEAR
                GL_NEAREST_MIPMAP_NEAREST       = #2700,
                GL_LINEAR_MIPMAP_NEAREST        = #2701,
                GL_NEAREST_MIPMAP_LINEAR        = #2702,
                GL_LINEAR_MIPMAP_LINEAR         = #2703,

                -- TextureParameterName
                GL_TEXTURE_MAG_FILTER           = #2800,
                GL_TEXTURE_MIN_FILTER           = #2801,
                GL_TEXTURE_WRAP_S               = #2802,
                GL_TEXTURE_WRAP_T               = #2803,
--      GL_TEXTURE_BORDER_COLOR
--      GL_TEXTURE_PRIORITY

                -- TextureTarget
--      GL_TEXTURE_1D
--      GL_TEXTURE_2D
                GL_PROXY_TEXTURE_1D             = #8063,
                GL_PROXY_TEXTURE_2D             = #8064,

                -- TextureWrapMode
                GL_CLAMP_TO_BORDER              = #812D,
                GL_CLAMP_TO_EDGE                = #812F,
                GL_CLAMP                        = #2900,
                GL_REPEAT                       = #2901,

                -- PixelInternalFormat
                GL_R3_G3_B2                     = #2A10,
                GL_ALPHA4                       = #803B,
                GL_ALPHA8                       = #803C,
                GL_ALPHA12                      = #803D,
                GL_ALPHA16                      = #803E,
                GL_LUMINANCE4                   = #803F,
                GL_LUMINANCE8                   = #8040,
                GL_LUMINANCE12                  = #8041,
                GL_LUMINANCE16                  = #8042,
                GL_LUMINANCE4_ALPHA4            = #8043,
                GL_LUMINANCE6_ALPHA2            = #8044,
                GL_LUMINANCE8_ALPHA8            = #8045,
                GL_LUMINANCE12_ALPHA4           = #8046,
                GL_LUMINANCE12_ALPHA12          = #8047,
                GL_LUMINANCE16_ALPHA16          = #8048,
                GL_INTENSITY                    = #8049,
                GL_INTENSITY4                   = #804A,
                GL_INTENSITY8                   = #804B,
                GL_INTENSITY12                  = #804C,
                GL_INTENSITY16                  = #804D,
                GL_RGB4                         = #804F,
                GL_RGB5                         = #8050,
                GL_RGB8                         = #8051,
                GL_RGB10                        = #8052,
                GL_RGB12                        = #8053,
                GL_RGB16                        = #8054,
                GL_RGBA2                        = #8055,
                GL_RGBA4                        = #8056,
                GL_RGB5_A1                      = #8057,
                GL_RGBA8                        = #8058,
                GL_RGB10_A2                     = #8059,
                GL_RGBA12                       = #805A,
                GL_RGBA16                       = #805B,
--      GL_COLOR_INDEX1_EXT
--      GL_COLOR_INDEX2_EXT
--      GL_COLOR_INDEX4_EXT
--      GL_COLOR_INDEX8_EXT
--      GL_COLOR_INDEX12_EXT
--      GL_COLOR_INDEX16_EXT

                -- InterleavedArrayFormat
                GL_V2F                          = #2A20,
                GL_V3F                          = #2A21,
                GL_C4UB_V2F                     = #2A22,
                GL_C4UB_V3F                     = #2A23,
                GL_C3F_V3F                      = #2A24,
                GL_N3F_V3F                      = #2A25,
                GL_C4F_N3F_V3F                  = #2A26,
                GL_T2F_V3F                      = #2A27,
                GL_T4F_V4F                      = #2A28,
                GL_T2F_C4UB_V3F                 = #2A29,
                GL_T2F_C3F_V3F                  = #2A2A,
                GL_T2F_N3F_V3F                  = #2A2B,
                GL_T2F_C4F_N3F_V3F              = #2A2C,
                GL_T4F_C4F_N3F_V4F              = #2A2D,
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
                GL_CLIP_PLANE0                  = #3000,
                GL_CLIP_PLANE1                  = #3001,
                GL_CLIP_PLANE2                  = #3002,
                GL_CLIP_PLANE3                  = #3003,
                GL_CLIP_PLANE4                  = #3004,
                GL_CLIP_PLANE5                  = #3005,

                -- EXT_abgr
                GL_ABGR_EXT                     = #8000,

                -- EXT_packed_pixels
                GL_UNSIGNED_BYTE_3_3_2_EXT      = #8032,
                GL_UNSIGNED_SHORT_4_4_4_4_EXT   = #8033,
                GL_UNSIGNED_SHORT_5_5_5_1_EXT   = #8034,
                GL_UNSIGNED_INT_8_8_8_8_EXT     = #8035,
                GL_UNSIGNED_INT_10_10_10_2_EXT  = #8036,

                -- EXT_vertex_array
                GL_VERTEX_ARRAY_EXT                 = #8074,
                GL_NORMAL_ARRAY_EXT                 = #8075,
                GL_COLOR_ARRAY_EXT                  = #8076,
                GL_INDEX_ARRAY_EXT                  = #8077,
                GL_TEXTURE_COORD_ARRAY_EXT          = #8078,
                GL_EDGE_FLAG_ARRAY_EXT              = #8079,
                GL_VERTEX_ARRAY_SIZE_EXT            = #807A,
                GL_VERTEX_ARRAY_TYPE_EXT            = #807B,
                GL_VERTEX_ARRAY_STRIDE_EXT          = #807C,
                GL_VERTEX_ARRAY_COUNT_EXT           = #807D,
                GL_NORMAL_ARRAY_TYPE_EXT            = #807E,
                GL_NORMAL_ARRAY_STRIDE_EXT          = #807F,
                GL_NORMAL_ARRAY_COUNT_EXT           = #8080,
                GL_COLOR_ARRAY_SIZE_EXT             = #8081,
                GL_COLOR_ARRAY_TYPE_EXT             = #8082,
                GL_COLOR_ARRAY_STRIDE_EXT           = #8083,
                GL_COLOR_ARRAY_COUNT_EXT            = #8084,
                GL_INDEX_ARRAY_TYPE_EXT             = #8085,
                GL_INDEX_ARRAY_STRIDE_EXT           = #8086,
                GL_INDEX_ARRAY_COUNT_EXT            = #8087,
                GL_TEXTURE_COORD_ARRAY_SIZE_EXT     = #8088,
                GL_TEXTURE_COORD_ARRAY_TYPE_EXT     = #8089,
                GL_TEXTURE_COORD_ARRAY_STRIDE_EXT   = #808A,
                GL_TEXTURE_COORD_ARRAY_COUNT_EXT    = #808B,
                GL_EDGE_FLAG_ARRAY_STRIDE_EXT       = #808C,
                GL_EDGE_FLAG_ARRAY_COUNT_EXT        = #808D,
                GL_VERTEX_ARRAY_POINTER_EXT         = #808E,
                GL_NORMAL_ARRAY_POINTER_EXT         = #808F,
                GL_COLOR_ARRAY_POINTER_EXT          = #8090,
                GL_INDEX_ARRAY_POINTER_EXT          = #8091,
                GL_TEXTURE_COORD_ARRAY_POINTER_EXT  = #8092,
                GL_EDGE_FLAG_ARRAY_POINTER_EXT      = #8093,

                -- EXT_color_table
                GL_TABLE_TOO_LARGE_EXT              = #8031,
                GL_COLOR_TABLE_FORMAT_EXT           = #80D8,
                GL_COLOR_TABLE_WIDTH_EXT            = #80D9,
                GL_COLOR_TABLE_RED_SIZE_EXT         = #80DA,
                GL_COLOR_TABLE_GREEN_SIZE_EXT       = #80DB,
                GL_COLOR_TABLE_BLUE_SIZE_EXT        = #80DC,
                GL_COLOR_TABLE_ALPHA_SIZE_EXT       = #80DD,
                GL_COLOR_TABLE_LUMINANCE_SIZE_EXT   = #80DE,
                GL_COLOR_TABLE_INTENSITY_SIZE_EXT   = #80DF,

                -- EXT_bgra
                GL_BGR_EXT                          = #80E0,
                GL_BGRA_EXT                         = #80E1,
                GL_BGRA                             = #80E1,

                -- EXT_paletted_texture
                GL_COLOR_INDEX1_EXT                 = #80E2,
                GL_COLOR_INDEX2_EXT                 = #80E3,
                GL_COLOR_INDEX4_EXT                 = #80E4,
                GL_COLOR_INDEX8_EXT                 = #80E5,
                GL_COLOR_INDEX12_EXT                = #80E6,
                GL_COLOR_INDEX16_EXT                = #80E7,

                -- SGI_compiled_vertex_array
                GL_ARRAY_ELEMENT_LOCK_FIRST_SGI     = #81A8,
                GL_ARRAY_ELEMENT_LOCK_COUNT_SGI     = #81A9,

                -- SGI_cull_vertex
                GL_CULL_VERTEX_SGI                  = #81AA,
                GL_CULL_VERTEX_EYE_POSITION_SGI     = #81AB,
                GL_CULL_VERTEX_OBJECT_POSITION_SGI  = #81AC,

                -- SGI_index_array_formats
                GL_IUI_V2F_SGI                      = #81AD,
                GL_IUI_V3F_SGI                      = #81AE,
                GL_IUI_N3F_V2F_SGI                  = #81AF,
                GL_IUI_N3F_V3F_SGI                  = #81B0,
                GL_T2F_IUI_V2F_SGI                  = #81B1,
                GL_T2F_IUI_V3F_SGI                  = #81B2,
                GL_T2F_IUI_N3F_V2F_SGI              = #81B3,
                GL_T2F_IUI_N3F_V3F_SGI              = #81B4,

                -- SGI_index_func
                GL_INDEX_TEST_SGI                   = #81B5,
                GL_INDEX_TEST_FUNC_SGI              = #81B6,
                GL_INDEX_TEST_REF_SGI               = #81B7,

                -- SGI_index_material
                GL_INDEX_MATERIAL_SGI               = #81B8,
                GL_INDEX_MATERIAL_PARAMETER_SGI     = #81B9,
                GL_INDEX_MATERIAL_FACE_SGI          = #81BA,

                GL_FOG_COORDINATE_SOURCE_EXT        = #8450,
                GL_FOG_COORDINATE_EXT               = #8451,
                GL_FRAGMENT_DEPTH_EXT               = #8452,
                GL_CURRENT_FOG_COORDINATE_EXT       = #8453,
                GL_FOG_COORDINATE_ARRAY_TYPE_EXT    = #8454,
                GL_FOG_COORDINATE_ARRAY_STRIDE_EXT  = #8455,
                GL_FOG_COORDINATE_ARRAY_POINTER_EXT = #8456,
                GL_FOG_COORDINATE_ARRAY_EXT         = #8457,

                GL_TEXTURE0                         = #84C0,
                GL_ELEMENT_ARRAY_BUFFER             = #8893,

                PFD_DOUBLEBUFFER   = 0x00000001,    --  The buffer is double-buffered. mutually exclusive to PFD_SUPPORT_GDI.
                PFD_DRAW_TO_WINDOW = 0x00000004,    --  The buffer can draw to a window or device surface.
--              PFD_DRAW_TO_BITMAP = 0x00000008,    --  The buffer can draw to a memory bitmap.
                PFD_SUPPORT_OPENGL = 0x00000020,    --  The buffer supports OpenGL drawing.
                PFD_TYPE_RGBA = 0,  -- RGBA pixels. Each pixel has four components in this order: red, green, blue, and alpha.
                PFD_MAIN_PLANE = 0,

                WGL_CONTEXT_MAJOR_VERSION_ARB       =   0x2091,
                WGL_CONTEXT_MINOR_VERSION_ARB       =   0x2092,
--              WGL_CONTEXT_LAYER_PLANE_ARB         =   0x2093,
--              WGL_CONTEXT_FLAGS_ARB               =   0x2094,
                WGL_CONTEXT_PROFILE_MASK_ARB        =   0x9126,
                WGL_CONTEXT_CORE_PROFILE_BIT_ARB    =   0x00000001,

$
--***********************************************************



--*************** Link functions from OpenGl32.dll **********
string dll_so = iff(platform()=LINUX?GL_LIBPATH & "libGL.so":"opengl32.dll")
atom opengl32 = open_dll(dll_so)

constant
xglAccum                = define_c_proc(opengl32,"glAccum",{GLenum,GLfloat}),
xglBegin                = define_c_proc(opengl32,"glBegin",{C_UINT}),
xglBindTexture          = define_c_proc(opengl32,"glBindTexture",{C_INT,C_UINT}),
xglBitmap               = define_c_proc(opengl32,"glBitmap",{C_INT,C_INT,C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT,C_PTR}),
xglBlendFunc            = define_c_proc(opengl32,"glBlendFunc",{C_INT,C_INT}),
xglCallList             = define_c_proc(opengl32,"glCallList",{C_UINT}),
xglCallLists            = define_c_proc(opengl32,"glCallLists",{C_INT,C_INT,C_PTR}),
xglClear                = define_c_proc(opengl32,"glClear",{C_UINT}),
xglClearAccum           = define_c_proc(opengl32,"glClearAccum",{GLfloat,GLfloat,GLfloat,GLfloat}),
xglClearColor           = define_c_proc(opengl32,"glClearColor",{GLclampf,GLclampf,GLclampf,GLclampf}),
xglClearDepth           = define_c_proc(opengl32,"glClearDepth",{C_DOUBLE}),
--xglColor3b            = define_c_proc(opengl32,"glColor3b",{C_CHAR,C_CHAR,C_CHAR}),
xglColor3d              = define_c_proc(opengl32,"glColor3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglColor3f            = define_c_proc(opengl32,"glColor3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--xglColor3i            = define_c_proc(opengl32,"glColor3i",{C_INT,C_INT,C_INT}),
--xglColor4b            = define_c_proc(opengl32,"glColor4b",{C_CHAR,C_CHAR,C_CHAR,C_CHAR}),
xglColor4d              = define_c_proc(opengl32,"glColor4d",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglColor4f            = define_c_proc(opengl32,"glColor4f",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--xglColor4i            = define_c_proc(opengl32,"glColor4i",{C_INT,C_INT,C_INT,C_INT}),
xglColorPointer         = define_c_proc(opengl32,"glColorPointer", {C_INT, C_UINT, C_UINT, C_PTR}),
xglCullFace             = define_c_proc(opengl32,"glCullFace",{C_INT}),
xglDeleteLists          = define_c_proc(opengl32,"glDeleteLists",{C_INT,C_INT}),
xglDepthFunc            = define_c_proc(opengl32,"glDepthFunc",{C_INT}),
xglDepthMask            = define_c_proc(opengl32,"glDepthMask", {C_CHAR}),
xglDisable              = define_c_proc(opengl32,"glDisable",{C_UINT}),
xglDisableClientState   = define_c_proc(opengl32,"glDisableClientState", {C_UINT}),
xglDrawElements         = define_c_proc(opengl32,"glDrawElements", {C_UINT, C_INT, C_UINT, C_PTR}),
xglEnable               = define_c_proc(opengl32,"glEnable",{C_UINT}),
xglEnableClientState    = define_c_proc(opengl32,"glEnableClientState", {C_UINT}),
xglEnd                  = define_c_proc(opengl32,"glEnd",{}),
xglEndList              = define_c_proc(opengl32,"glEndList",{}),
--xglEvalMesh2          = define_c_proc(opengl32,"glEvalMesh2",repeat(C_INT,5)),
xglFinish               = define_c_proc(opengl32,"glFinish",{}),
xglFlush                = define_c_proc(opengl32,"glFlush",{}),
xglFogf                 = define_c_proc(opengl32,"glFogf",{GLenum,GLfloat}),
xglFogfv                = define_c_proc(opengl32,"glFogfv",{C_INT,C_PTR}),
xglFogi                 = define_c_proc(opengl32,"glFogi",{C_INT,C_INT}),
--xglFogiv              = define_c_proc(opengl32,"glFogiv",{C_INT,C_PTR}),
xglFrontFace            = define_c_proc(opengl32,"glFrontFace",{C_INT}),
xglFrustum              = define_c_proc(opengl32,"glFrustum",repeat(C_DOUBLE,6)),
xglGenLists             = define_c_func(opengl32,"glGenLists",{C_INT},C_INT),
xglGetError             = define_c_func(opengl32,"glGetError",{},C_INT),
xglGetBooleanv          = define_c_proc(opengl32,"glGetBooleanv", {C_UINT, C_PTR}),
xglGetDoublev           = define_c_proc(opengl32,"glGetDoublev", {C_UINT, C_PTR}),
xglGetFloatv            = define_c_proc(opengl32,"glGetFloatv", {C_UINT, C_PTR}),
xglGetIntegerv          = define_c_proc(opengl32,"glGetIntegerv", {C_UINT, C_PTR}),
xglGetString            = define_c_func(opengl32,"glGetString", {C_UINT}, C_PTR),
xglHint                 = define_c_proc(opengl32,"glHint",{C_INT,C_INT}),
xglIndexi               = define_c_proc(opengl32,"glIndexi",{C_INT}),
xglLightf               = define_c_proc(opengl32,"glLightf",{GLenum,GLenum,GLfloat}),
xglLightfv              = define_c_proc(opengl32,"glLightfv",{C_INT,C_INT,C_PTR}),
xglLightModelf          = define_c_proc(opengl32,"glLightModelf",{C_INT,C_FLOAT}),
xglLightModelfv         = define_c_proc(opengl32,"glLightModelfv",{C_INT,C_PTR}),
xglListBase             = define_c_proc(opengl32,"glListBase",{C_UINT}),
xglLoadIdentity         = define_c_proc(opengl32,"glLoadIdentity",{}),
xglLoadMatrixd          = define_c_proc(opengl32,"glLoadMatrixd",{C_PTR}),
--xglMap2f              = define_c_proc(opengl32,"glMap2f",{GLenum,GLfloat,GLfloat,GLint,GLint,GLfloat,GLfloat,GLint,GLint,C_PTR}),
--xglMapGrid2f          = define_c_proc(opengl32,"glMapGrid2f",{C_INT,C_FLOAT,C_FLOAT,C_INT,C_FLOAT,C_INT}),
xglMaterialf            = define_c_proc(opengl32,"glMaterialf",{C_UINT,C_UINT,C_FLOAT}),
xglMaterialfv           = define_c_proc(opengl32,"glMaterialfv",{C_INT,C_INT,C_PTR}),
--xglMateriali          = define_c_proc(opengl32,"glMateriali",{C_UINT,C_UINT,C_INT}),
xglMatrixMode           = define_c_proc(opengl32,"glMatrixMode",{C_UINT}),
xglNewList              = define_c_proc(opengl32,"glNewList",{C_UINT,C_INT}),
--xglNormal3b           = define_c_proc(opengl32,"glNormal3b",{C_CHAR,C_CHAR,C_CHAR}),
xglNormal3d             = define_c_proc(opengl32,"glNormal3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglNormal3f           = define_c_proc(opengl32,"glNormal3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--xglNormal3i           = define_c_proc(opengl32,"glNormal3i",{C_INT,C_INT,C_INT}),
--xglNormal3s           = define_c_proc(opengl32,"glNormal3s",{C_SHORT,C_SHORT,C_SHORT}),
xglOrtho                = define_c_proc(opengl32,"glOrtho",repeat(C_DOUBLE,6)),
xglPixelStorei          = define_c_proc(opengl32,"glPixelStorei",{C_INT,C_INT}),
xglPointSize            = define_c_proc(opengl32,"glPointSize",{C_FLOAT}),
xglPolygonMode          = define_c_proc(opengl32,"glPolygonMode",{C_INT,C_INT}),
xglPopMatrix            = define_c_proc(opengl32,"glPopMatrix",{}),
xglPushMatrix           = define_c_proc(opengl32,"glPushMatrix",{}),
xglRasterPos2i          = define_c_proc(opengl32,"glRasterPos2i",{C_INT,C_INT}),
--xglReadBuffer         = define_c_proc(opengl32,"glReadBuffer",{C_INT}),
--xglReadPixels         = define_c_proc(opengl32,"glReadPixels",repeat(C_INT,6) & C_PTR),
--xglRectd              = define_c_proc(opengl32,"glRectd",repeat(C_DOUBLE,4)),
--xglRectdv             = define_c_proc(opengl32,"glRectdv",{C_PTR}),
xglRectf                = define_c_proc(opengl32,"glRectf",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--xglRectfv             = define_c_proc(opengl32,"glRectfv",{C_PTR}),
--xglRecti              = define_c_proc(opengl32,"glRecti",repeat(C_INT,4)),
--xglRectiv             = define_c_proc(opengl32,"glRectiv",{C_PTR}),
--xglRects              = define_c_proc(opengl32,"glRects",repeat(C_SHORT,4)),
--xglRectsv             = define_c_proc(opengl32,"glRectsv",{C_PTR}),
--xglRenderMode         = define_c_func(opengl32,"glRenderMode",{C_INT},C_INT),
xglRotated              = define_c_proc(opengl32,"glRotated",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
xglRotatef              = define_c_proc(opengl32,"glRotatef",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
xglScaled               = define_c_proc(opengl32,"glScaled",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
xglScalef               = define_c_proc(opengl32,"glScalef",{C_FLOAT,C_FLOAT,C_FLOAT}),
xglScissor              = define_c_proc(opengl32,"glScissor",repeat(C_INT,4)),
--xglSelectBuffer       = define_c_proc(opengl32,"glSelectBuffer",{C_INT,C_PTR}),
xglShadeModel           = define_c_proc(opengl32,"glShadeModel",{C_UINT}),
--xglStencilFunc        = define_c_proc(opengl32,"glStencilFunc",{C_INT,C_INT,C_UINT}),
--xglStencilMask        = define_c_proc(opengl32,"glStencilMask",{C_UINT}),
--xglStencilOp          = define_c_proc(opengl32,"glStencilOp",{C_INT,C_INT,C_INT}),
--xglTexCoord1d         = define_c_proc(opengl32,"glTexCoord1d",{C_DOUBLE}),
--xglTexCoord1dv        = define_c_proc(opengl32,"glTexCoord1dv",{C_PTR}),
--xglTexCoord1fv        = define_c_proc(opengl32,"glTexCoord1fv",{C_PTR}),
--xglTexCoord1i         = define_c_proc(opengl32,"glTexCoord1i",{C_INT}),
--xglTexCoord1iv        = define_c_proc(opengl32,"glTexCoord1iv",{C_PTR}),
--xglTexCoord1s         = define_c_proc(opengl32,"glTexCoord1s",{C_SHORT}),
--xglTexCoord1sv        = define_c_proc(opengl32,"glTexCoord1sv",{C_PTR}),
--xglTexCoord2d         = define_c_proc(opengl32,"glTexCoord2d",{C_DOUBLE,C_DOUBLE}),
--xglTexCoord2dv        = define_c_proc(opengl32,"glTexCoord2dv",{C_PTR}),
--xglTexCoord2f         = define_c_proc(opengl32,"glTexCoord2f",{C_FLOAT,C_FLOAT}),
--xglTexCoord2fv        = define_c_proc(opengl32,"glTexCoord2fv",{C_PTR}),
--xglTexCoord2i         = define_c_proc(opengl32,"glTexCoord2i",{C_INT,C_INT}),
--xglTexCoord2iv        = define_c_proc(opengl32,"glTexCoord2iv",{C_PTR}),
--xglTexCoord2s         = define_c_proc(opengl32,"glTexCoord2s",{C_SHORT,C_SHORT}),
--xglTexCoord2sv        = define_c_proc(opengl32,"glTexCoord2sv",{C_PTR}),
--xglTexCoord3d         = define_c_proc(opengl32,"glTexCoord3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglTexCoord3dv        = define_c_proc(opengl32,"glTexCoord3dv",{C_PTR}),
--xglTexCoord3f         = define_c_proc(opengl32,"glTexCoord3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--xglTexCoord3fv        = define_c_proc(opengl32,"glTexCoord3fv",{C_PTR}),
--xglTexCoord3i         = define_c_proc(opengl32,"glTexCoord3i",{C_INT,C_INT,C_INT}),
--xglTexCoord3iv        = define_c_proc(opengl32,"glTexCoord3iv",{C_PTR}),
--xglTexCoord3s         = define_c_proc(opengl32,"glTexCoord3s",{C_SHORT,C_SHORT,C_SHORT}),
--xglTexCoord3sv        = define_c_proc(opengl32,"glTexCoord3sv",{C_PTR}),
xglTexCoord4d           = define_c_proc(opengl32,"glTexCoord4d",repeat(C_DOUBLE,4)),
--xglTexCoord4dv        = define_c_proc(opengl32,"glTexCoord4dv",{C_PTR}),
--xglTexCoord4fv        = define_c_proc(opengl32,"glTexCoord4fv",{C_PTR}),
--xglTexCoord4i         = define_c_proc(opengl32,"glTexCoord4i",repeat(C_INT,4)),
--xglTexCoord4iv        = define_c_proc(opengl32,"glTexCoord4iv",{C_PTR}),
--xglTexCoord4s         = define_c_proc(opengl32,"glTexCoord4s",repeat(C_SHORT,4)),
--xglTexCoord4sv        = define_c_proc(opengl32,"glTexCoord4sv",{C_PTR}),
xglTexEnvi              = define_c_proc(opengl32,"glTexEnvi",{C_INT,C_INT,C_INT}),
xglTexEnvf              = define_c_proc(opengl32,"glTexEnvf",{C_INT,C_INT,C_FLOAT}),
--xglTexEnvf            = define_c_proc(opengl32,"glTexEnvf",{C_INT,C_INT,C_DOUBLE}),
xglTexGeni              = define_c_proc(opengl32,"glTexGeni",{C_INT,C_INT,C_INT}),
--xglTexImage1D         = define_c_proc(opengl32,"glTexImage1D",repeat(C_INT,7) & C_PTR),
xglTexImage2D           = define_c_proc(opengl32,"glTexImage2D",repeat(C_INT,8) & C_PTR),
--xglTexParameterfv     = define_c_proc(opengl32,"glTexParameterfv",{C_INT,C_INT,C_PTR}),
xglTexParameteri        = define_c_proc(opengl32,"glTexParameteri",{C_INT,C_INT,C_INT}),
--xglTexParameteriv     = define_c_proc(opengl32,"glTexParameteriv",{C_INT,C_INT,C_PTR}),
xglTranslated           = define_c_proc(opengl32,"glTranslated",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
xglTranslatef           = define_c_proc(opengl32,"glTranslatef",{C_FLOAT,C_FLOAT,C_FLOAT}),
--glUniform
--xglVertex2d           = define_c_proc(opengl32,"glVertex2d",{C_DOUBLE,C_DOUBLE}),
--xglVertex2dv          = define_c_proc(opengl32,"glVertex2dv",{C_PTR}),
--xglVertex2f           = define_c_proc(opengl32,"glVertex2f",{C_FLOAT,C_FLOAT}),
--xglVertex2fv          = define_c_proc(opengl32,"glVertex2fv",{C_PTR}),
--xglVertex2i           = define_c_proc(opengl32,"glVertex2i",{C_INT,C_INT}),
--xglVertex2iv          = define_c_proc(opengl32,"glVertex2iv",{C_PTR}),
--xglVertex2s           = define_c_proc(opengl32,"glVertex2s",{C_SHORT,C_SHORT}),
--xglVertex2sv          = define_c_proc(opengl32,"glVertex2sv",{C_PTR}),
xglVertex3d             = define_c_proc(opengl32,"glVertex3d",{C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglVertex3dv          = define_c_proc(opengl32,"glVertex3dv",{C_PTR}),
--xglVertex3f           = define_c_proc(opengl32,"glVertex3f",{C_FLOAT,C_FLOAT,C_FLOAT}),
--xglVertex3fv          = define_c_proc(opengl32,"glVertex3fv",{C_PTR}),
--xglVertex3i           = define_c_proc(opengl32,"glVertex3i",{C_INT,C_INT,C_INT}),
--xglVertex3iv          = define_c_proc(opengl32,"glVertex3iv",{C_PTR}),
--xglVertex3s           = define_c_proc(opengl32,"glVertex3s",{C_SHORT,C_SHORT,C_SHORT}),
--xglVertex3sv          = define_c_proc(opengl32,"glVertex3sv",{C_PTR}),
xglVertex4d             = define_c_proc(opengl32,"glVertex4d",{C_DOUBLE,C_DOUBLE,C_DOUBLE,C_DOUBLE}),
--xglVertex4dv          = define_c_proc(opengl32,"glVertex4dv",{C_PTR}),
--xglVertex4fv          = define_c_proc(opengl32,"glVertex4fv",{C_PTR}),
--xglVertex4f           = define_c_proc(opengl32,"glVertex4f",{C_FLOAT,C_FLOAT,C_FLOAT,C_FLOAT}),
--xglVertex4i           = define_c_proc(opengl32,"glVertex4i",{C_INT,C_INT,C_INT,C_INT}),
--xglVertex4iv          = define_c_proc(opengl32,"glVertex4iv",{C_PTR}),
--xglVertex4s           = define_c_proc(opengl32,"glVertex4s",{C_SHORT,C_SHORT,C_SHORT,C_SHORT}),
--xglVertex4sv          = define_c_proc(opengl32,"glVertex4sv",{C_PTR}),
xglVertexPointer        = define_c_proc(opengl32,"glVertexPointer", {C_INT, C_UINT, C_UINT, C_PTR}),
xglViewport             = define_c_proc(opengl32,"glViewport",{C_INT,C_INT,C_INT,C_INT}),
--WglGetProcAddress     = define_c_func(opengl32,"wglGetProcAddress", {C_PTR}, C_PTR),
sglGetProcAddress       = iff(platform()=WINDOWS?"wglGetProcAddress":"glXGetProcAddress"),
xglGetProcAddress       = define_c_func(opengl32,sglGetProcAddress, {C_PTR}, C_PTR),
xwglMakeCurrent         = define_c_func(opengl32,"wglMakeCurrent", {C_PTR, C_PTR},C_BOOL),
--BOOL wglMakeCurrent(
--  HDC unnamedParam1,
--  HGLRC unnamedParam2
--);
xwglDeleteContext       = define_c_func(opengl32,"wglDeleteContext", {C_PTR},C_BOOL)
--BOOL wglDeleteContext(
--  HGLRC unnamedParam1
--);
--WglUseFontOutlines        = define_c_func(opengl32,"wglUseFontOutlinesA",{C_UINT,C_INT,C_INT,C_INT,C_FLOAT,C_FLOAT,C_INT,C_PTR},C_INT)
--xwglCreateContext     = define_c_func(opengl32,"wglCreateContext",{C_INT},C_INT)

global constant 
        idPIXELFORMATDESCRIPTOR = define_struct(`typedef struct tagPIXELFORMATDESCRIPTOR {
                                                    WORD  nSize;
                                                    WORD  nVersion;
                                                    DWORD dwFlags;
                                                    BYTE  iPixelType;
                                                    BYTE  cColorBits;
                                                    BYTE  cRedBits;
                                                    BYTE  cRedShift;
                                                    BYTE  cGreenBits;
                                                    BYTE  cGreenShift;
                                                    BYTE  cBlueBits;
                                                    BYTE  cBlueShift;
                                                    BYTE  cAlphaBits;
                                                    BYTE  cAlphaShift;
                                                    BYTE  cAccumBits;
                                                    BYTE  cAccumRedBits;
                                                    BYTE  cAccumGreenBits;
                                                    BYTE  cAccumBlueBits;
                                                    BYTE  cAccumAlphaBits;
                                                    BYTE  cDepthBits;
                                                    BYTE  cStencilBits;
                                                    BYTE  cAuxBuffers;
                                                    BYTE  iLayerType;
                                                    BYTE  bReserved;
                                                    DWORD dwLayerMask;
                                                    DWORD dwVisibleMask;
                                                    DWORD dwDamageMask;
                                                 } PIXELFORMATDESCRIPTOR, *PPIXELFORMATDESCRIPTOR, *LPPIXELFORMATDESCRIPTOR;`)

atom gdi32 = NULL

local procedure open_gdi32()
    gdi32 = open_dll("gdi32.dll")
end procedure

integer xChoosePixelFormat = NULL

--local function glChoosePixelFormat(atom hdc, pfd)
global function glChoosePixelFormat(atom hdc, pfd)
    if platform()!=WINDOWS then ?9/0 end if -- placeholder?? (?glXChooseVisual?)
    if xChoosePixelFormat=NULL then
        if gdi32=NULL then open_gdi32() end if
        xChoosePixelFormat = define_c_func(gdi32,"ChoosePixelFormat",
            {C_PTR,     --  HDC hdc
             C_PTR},    --  const PIXELFORMATDESCRIPTOR* ppfd
            C_INT)      -- int
    end if
    integer res = c_func(xChoosePixelFormat,{hdc,pfd})
    assert(res!=0)
    return res
end function

integer xSetPixelFormat = NULL

global procedure glSetPixelFormat(atom hDC, integer fmt=0, atom pfd=NULL)
    bool bFree = fmt<=0 and pfd=NULL
    if bFree then
        pfd = allocate_struct(idPIXELFORMATDESCRIPTOR)
--/*
int pfdAttribs[] = {
--  PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER | PFD_DRAW_TO_PBUFFER,
--  PFD_TYPE_RGBA, 32,          // colour bits
--  24,                         // depth bits
--  0,                          // accumulation
--  8,                          // stencil
--  PFD_MAIN_PLANE,
--  0,0,0,0
};
By PFD_DRAW_TO_PBUFFER did you mean PFD_DRAW_TO_BITMAP, and should that be used with or instead of PFD_DRAW_TO_WINDOW?
--Can cost 0.5–2 ms (or more on integrated GPUs) per frame, destroying real-time performance if done every frame.
OK, instead of any of that, can we continue to let OpenGl draw directly to screen, this time the client area less a
5 pixel border, but create a client-area-sized bitmap as per the original, with a cream 5px border with a single black
line in the middle, and a transparent central part, and BitBlit that over/after the OpenGL-drawn parts?
--*/
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"nSize",get_struct_size(idPIXELFORMATDESCRIPTOR))
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"nVersion",1)
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"dwFlags",or_all({PFD_DRAW_TO_WINDOW,PFD_SUPPORT_OPENGL,PFD_DOUBLEBUFFER}))
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"iPixelType",PFD_TYPE_RGBA)
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"cColorBits",32)
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"cDepthBits",24)
        set_struct_field(idPIXELFORMATDESCRIPTOR,pfd,"iLayerType",PFD_MAIN_PLANE)
        fmt = glChoosePixelFormat(hDC, pfd)
    end if
    if xSetPixelFormat=NULL then
        if gdi32=NULL then open_gdi32() end if
        xSetPixelFormat = define_c_func(gdi32,"SetPixelFormat",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int format
             C_PTR},    --  const PIXELFORMATDESCRIPTOR* ppfd
            C_INT)      -- BOOL
    end if
    bool res = c_func(xSetPixelFormat,{hDC,fmt,pfd})
    assert(res)
    if bFree then free(pfd) end if
end procedure

integer xSwapBuffers = NULL

global procedure glSwapBuffers(atom hDC)
--glXSwapBuffers?
    if xSwapBuffers=NULL then
        if gdi32=NULL then open_gdi32() end if
        xSwapBuffers = define_c_func(gdi32,"SwapBuffers",
            {C_PTR},    --  HDC unnamedParam1
            C_INT)      -- BOOL
    end if
    bool res = c_func(xSwapBuffers,{hDC})
    assert(res)
end procedure

--1/12/16 (WglUseFontOutlines is Windows-only)
atom WglUseFontOutlines = NULL
global function wglUseFontOutlines(atom glhDC, integer first, integer count, atom pFontList, atom deviation, atom extrusion, integer fmt, atom pGMF)
    if WglUseFontOutlines=NULL then
        if platform()!=WINDOWS then ?9/0 end if
        WglUseFontOutlines = define_c_func(opengl32,"wglUseFontOutlinesA",{C_UINT,C_INT,C_INT,C_INT,C_FLOAT,C_FLOAT,C_INT,C_PTR},C_INT)
    end if
    return c_func(WglUseFontOutlines,{glhDC,first,count,pFontList,deviation,extrusion,fmt,pGMF})
end function

--constant gl_vector_buffer = allocate(16384)    
constant gl_vector_buffer = allocate(128)

global function wglGetProcAddress(string name)
    atom addr = c_func(xglGetProcAddress,{name})
--29/11/21
--  if addr<=0 then
--      crash("Couldn't find " & name)
--  end if
    return addr
end function

function link_glext_func(string name, sequence args, atom result)
    atom addr = wglGetProcAddress(name)
    if addr<=3 then
        return define_c_func(opengl32, name, args, result)
    end if
    return define_c_func({},addr,args,result)
end function

function link_glext_proc(string name, sequence args)
    atom addr = wglGetProcAddress(name)
    if addr<=3 then
        return define_c_proc(opengl32, name, args)
    end if
    return define_c_proc({},addr,args)
end function

procedure gl_pokef32(atom dest, sequence data)
    for i=1 to length(data) do
        poke(dest,atom_to_float32(data[i]))
        dest += 4
    end for
end procedure

global function glInt32Array(sequence data)
    integer size = length(data)*4
    atom pData = allocate(size)
--  sequence res = {size,pData}
--  for i=1 to length(data) do
--      poke4(pData,data[i])
--      pData += 4
--  end for
--  return res
    poke4(pData,data)
    return pData
end function

--/*
global function glFloat32Array(sequence data)
    integer size = length(data)*4
    atom pData = allocate(size)
    sequence res = {size,pData}
    for i=1 to length(data) do
        poke(pData,atom_to_float32(data[i]))
        pData += 4
    end for
    return res
end function
--*/
global function glFloat32Array(sequence data)
    integer size = length(data)*4
    bool bFlat = atom(data[1])
    if not bFlat then
        size *= length(data[1])
    end if
    atom pData = allocate(size)
--  sequence res = {size,pData}
    atom res = pData
    if bFlat then
        -- data must be in column major order, ie {{1,2},{3,4}} == {1,3,2,4}.
        for i=1 to length(data) do
            poke(pData,atom_to_float32(data[i]))
            pData += 4
        end for
    else
        for c=1 to length(data[1]) do
            for r=1 to length(data) do
                poke(pData,atom_to_float32(data[r][c]))
                pData += 4
            end for
        end for
    end if
    return res
end function


--/* (not [yet] used):
procedure gl_pokef64(atom dest,sequence data)
    for i=1 to length(data) do
        poke(dest,atom_to_float64(data[i]))
        dest += 8
    end for
end procedure
--*/

atom wglCreateContextAttribsARB = -1,
     pAttrib, xwglCreateContextAttribsARB,
     xwglCreateContext = NULL

global function wglCreateContext(atom hDC)
--glXCreateContext?
    -- Try to create a 3.3 core context (fallback to legacy if not supported)
    if wglCreateContextAttribsARB=-1 then
        -- wglCreateContextAttribsARB is an extension; fetch its address
        wglCreateContextAttribsARB = wglGetProcAddress("wglCreateContextAttribsARB")
        if wglCreateContextAttribsARB!=NULL then
            xwglCreateContextAttribsARB = define_c_func({},wglCreateContextAttribsARB,
                {C_PTR,     --  HDC hDC
                 C_PTR,     --  HGLRC hShareContext
                 C_PTR},    --  const int *attribList
                 C_PTR)     -- HGLRC
            pAttrib = glInt32Array({WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
                                    WGL_CONTEXT_MINOR_VERSION_ARB, 3,
                                    WGL_CONTEXT_PROFILE_MASK_ARB,  WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
                                    0})
        end if
    end if
    atom hRC
    if wglCreateContextAttribsARB!=NULL then
?9/0
        hRC = c_func(xwglCreateContextAttribsARB,{hDC,0,pAttrib})
    else
        if xwglCreateContext=NULL then
            xwglCreateContext = define_c_func(opengl32,"wglCreateContext",
                {C_PTR},    --  HDC unnamedParam1
                C_PTR)      -- HGLRC
        end if
        hRC = c_func(xwglCreateContext,{hDC})   -- legacy context (still works for GL 2.1)
    end if
    if hRC=0 then crash("wglCreateContext* failed") end if
    return hRC
end function

global function glGetError()
    return c_func(xglGetError,{})
end function

global procedure glAccum(integer op, atom val)
    c_proc(xglAccum,{op,val})
end procedure

integer xglActiveTexture = 0

global procedure glActiveTexture(integer texture)
    if xglActiveTexture=0 then
        xglActiveTexture = link_glext_proc("glActiveTexture",{GLuint})
    end if
    c_proc(xglActiveTexture,{texture})
end procedure

integer xglAttachShader = 0

global procedure glAttachShader(integer program, shader)
    if xglAttachShader=0 then
        xglAttachShader = link_glext_proc("glAttachShader",{GLuint,GLuint})
    end if
    c_proc(xglAttachShader,{program,shader})
end procedure

global procedure glBegin(integer what)
    c_proc(xglBegin,{what})
end procedure

integer xglBindAttribLocation = 0

global procedure glBindAttribLocation(integer program, index, string name)
    if xglBindAttribLocation=0 then
        xglBindAttribLocation = link_glext_proc("glBindAttribLocation",{GLuint,GLuint,C_PTR})
    end if
    c_proc(xglBindAttribLocation,{program,index,name})
end procedure

integer xglBindBuffer = 0

global procedure glBindBuffer(integer target, buffer)
    if xglBindBuffer=0 then
        xglBindBuffer = link_glext_proc("glBindBuffer",{GLenum,GLuint})
    end if
    c_proc(xglBindBuffer,{target,buffer})
end procedure

global procedure glBindTexture(integer target, atom texture)
    c_proc(xglBindTexture,{target,texture})
end procedure


integer xglBindVertexArray = 0

global procedure glBindVertexArray(atom vao)
    if xglBindVertexArray=0 then
        xglBindVertexArray = link_glext_proc("glBindVertexArray",{C_PTR})
--void glBindVertexArray( GLuint array);
    end if
    c_proc(xglBindVertexArray,{vao})
end procedure
 

global procedure glBitmap(integer width, height, atom xorig, yorig, xmove, ymove, bitmap)
    c_proc(xglBitmap,{width,height,xorig,yorig,xmove,ymove,bitmap})
end procedure

global procedure glBlendFunc(integer sfactor, dfactor)
    c_proc(xglBlendFunc,{sfactor,dfactor})
end procedure

integer xglBufferData = 0

--global procedure glBufferData(integer target, size, atom pData, integer usage)
global procedure glBufferData(integer target, object size, pData, usage=0)
    if sequence(size) then
        -- invoked as glBufferData(integer target, sequence data[in size], integer usage[in pData]):
        assert(usage==0)
        usage = pData
        if target==GL_ARRAY_BUFFER then
            pData = glFloat32Array(size)
        elsif target==GL_ELEMENT_ARRAY_BUFFER then
            pData = glInt32Array(size)
        else
            pData = 9/0
        end if
        size = length(size)*4
    end if
    assert(integer(size))
    assert(atom(pData))
--  assert(integer(usage))
    assert(usage==GL_STATIC_DRAW)
    if xglBufferData=0 then
        xglBufferData = link_glext_proc("glBufferData",{GLenum,C_INT,C_PTR,GLenum})
    end if
    c_proc(xglBufferData,{target, size, pData, usage})
end procedure

global procedure glCallList(integer list)
    c_proc(xglCallList,{list})
end procedure

global procedure glCallLists(integer n, integer typ, atom lists)
    c_proc(xglCallLists,{n,typ,lists})
end procedure

global procedure glClear(integer mask)
    c_proc(xglClear,{mask})
end procedure

global procedure glClearAccum(atom r, g, b, a)
    c_proc(xglClearAccum,{r,g,b,a})
end procedure

global procedure glClearColor(atom r, g, b, a)
    c_proc(xglClearColor,{r,g,b,a})
end procedure

global procedure glClearDepth(atom depth)
    c_proc(xglClearDepth,{depth})
end procedure

global procedure glColor(atom red, blue, green, alpha=1)
    c_proc(xglColor4d,{red,blue,green,alpha})
end procedure

--global procedure glColor3d(sequence col)
global procedure glColor3(sequence colour)
    if length(colour)=3 then
        c_proc(xglColor3d,colour)
    else
        c_proc(xglColor4d,colour)
    end if
end procedure

--global procedure glColor3f(sequence col)
--  c_proc(xglColor3f,col)
--end procedure

--global procedure glColor4f(sequence col)
--  c_proc(xglColor4f,col)
--end procedure

global procedure glColorPointer(integer size, integer ptype, integer stride, atom pointer)
    c_proc(xglColorPointer, {size, ptype, stride, pointer})
end procedure

--global function wglCreateContext(integer hDC)
--  integer context = c_func(xwglCreateContext,{hDC})
--  return context
--end function

integer xglCompileShader = 0

global procedure glCompileShader(integer shader)
    if xglCompileShader=0 then
        xglCompileShader = link_glext_proc("glCompileShader",{GLuint})
--void glCompileShader(GLuint shader);
    end if
    c_proc(xglCompileShader,{shader})
end procedure

atom pWord = allocate(machine_word())

integer xglGenBuffers = 0

global function glCreateBuffer()
    if xglGenBuffers=0 then
        xglGenBuffers = link_glext_proc("glGenBuffers",{GLuint,C_PTR})
    end if
    poken(pWord,0)
    c_proc(xglGenBuffers,{1,pWord})
    integer buffer = peekns(pWord)
    if buffer=0 then ?9/0 end if
    return buffer
end function

integer xglCreateProgram = 0

global function glCreateProgram()
    if xglCreateProgram=0 then
        xglCreateProgram = link_glext_func("glCreateProgram",{},GLuint)
    end if
    integer prog = c_func(xglCreateProgram,{})
    if prog=0 then ?9/0 end if
    return prog
end function

integer xglCreateShader = 0

global function glCreateShader(integer shader_type)
    if xglCreateShader=0 then
        -- note that wglGetProcAddress requires an OpenGL rendering context; 
        -- wglCreateContext and wglMakeCurrent must be called prior to this.
        -- IupGLMakeCurrent() should do all that for you, I think...
        -- (In other experiments I have also found that calling SDL_Init()
        --  and SDL_SetVideoMode() are required/enough to do beforehand.)
        xglCreateShader = link_glext_func("glCreateShader",{GLenum},GLuint)
    end if
    integer id = c_func(xglCreateShader,{shader_type})
    if id=0 then ?9/0 end if
    return id
end function

integer xglGenTextures =0

global function glCreateTexture()
    if xglGenTextures=0 then
        xglGenTextures = define_c_proc(opengl32,"glGenTextures",{C_INT,C_PTR})
    end if
    poken(pWord,0)
    c_proc(xglGenTextures,{1,pWord})
--  integer texture = peekns(pWord)
    integer texture = peek4u(pWord)
    if texture=0 then ?9/0 end if
    return texture
end function
--  integer e = glGetError()
--  if e!=GL_NO_ERROR then ?9/0 end if
--  return texture

global procedure glCullFace(integer mode)
    c_proc(xglCullFace,{mode})
end procedure

global procedure glDeleteLists(atom list,integer range)
    c_proc(xglDeleteLists,{list,range})
end procedure

integer xglDeleteProgram = 0

global function glDeleteProgram(integer program)
    if xglDeleteProgram=0 then
        xglDeleteProgram = link_glext_proc("glDeleteProgram",{GLuint})
    end if
    c_proc(xglDeleteProgram,{program})
    return 0
end function

integer xglDeleteShader = 0

global function glDeleteShader(integer shader)
    if xglDeleteShader=0 then
        xglDeleteShader = link_glext_proc("glDeleteShader",{GLuint})
--void glDeleteShader(GLuint shader);
    end if
    c_proc(xglDeleteShader,{shader})
    return 0 -- use shader = glDeleteShader(shader); prevent mishaps
end function

integer xglDeleteTextures = 0

global procedure glDeleteTexture(integer texture)
    if xglDeleteTextures=0 then
        xglDeleteTextures = link_glext_proc("glDeleteTextures",{GLuint,GLuint})
    end if
    poken(pWord,texture)
    c_proc(xglDeleteTextures,{1,pWord})
end procedure

integer xglDetachShader = 0

global procedure glDetachShader(integer program, shader)
    if xglDetachShader=0 then
        xglDetachShader = link_glext_proc("glDetachShader",{GLuint,GLuint})
    end if
    c_proc(xglDetachShader,{program,shader})
end procedure

global procedure glDepthFunc(integer f)
    c_proc(xglDepthFunc,{f})
end procedure

global procedure glDepthMask(integer flag)
    c_proc(xglDepthMask, {flag})
end procedure

global procedure glDisable(integer what)
    c_proc(xglDisable,{what})
end procedure

global procedure glDisableClientState(integer cap)
    c_proc(xglDisableClientState, {cap})
end procedure

integer xglDisableVertexAttribArray = 0

global procedure glDisableVertexAttribArray(integer index)
    if xglDisableVertexAttribArray=0 then
        xglDisableVertexAttribArray = link_glext_proc("glDisableVertexAttribArray",{GLuint})
    end if
    c_proc(xglDisableVertexAttribArray,{index})
end procedure

integer xglEnableVertexAttribArray = 0

global procedure glEnableVertexAttribArray(integer index)
    if xglEnableVertexAttribArray=0 then
        xglEnableVertexAttribArray = link_glext_proc("glEnableVertexAttribArray",{GLuint})
    end if
    c_proc(xglEnableVertexAttribArray,{index})
end procedure

integer xglDrawArrays = 0

global procedure glDrawArrays(integer mode, first, count)
    if xglDrawArrays=0 then
        xglDrawArrays = link_glext_proc("glDrawArrays",{GLenum,GLint,GLsizei})
    end if
    c_proc(xglDrawArrays,{mode, first, count})
end procedure

global procedure glDrawElements(integer mode, count, stype, atom indices)
    c_proc(xglDrawElements, {mode, count, stype, indices})
end procedure

global procedure glEnable(integer what)
    c_proc(xglEnable,{what})
end procedure

global procedure glEnableClientState(integer cap)
    c_proc(xglEnableClientState, {cap})
end procedure

global procedure glEnd()
    c_proc(xglEnd,{})
end procedure

global procedure glEndList()
    c_proc(xglEndList,{})
end procedure

global procedure glFinish()
    c_proc(xglFinish,{})
end procedure

global procedure glFlush()
    c_proc(xglFlush,{})
end procedure

global procedure glFogf(integer pname, atom param)
    c_proc(xglFogf,{pname,param})
end procedure

global procedure glFogfv(integer pname, sequence params)
    gl_pokef32(gl_vector_buffer,params)
    c_proc(xglFogfv,{pname,gl_vector_buffer})
end procedure

global procedure glFogi(integer pname, integer param)
    c_proc(xglFogi,{pname,param})
end procedure

global procedure glFrontFace(integer mode)
    c_proc(xglFrontFace,{mode})
end procedure

global procedure glFrustum(atom left, right, top, bottom, zNear, zFar)
    c_proc(xglFrustum,{left,right,top,bottom,zNear,zFar})
end procedure

integer xglGenerateMipmap = 0

global procedure glGenerateMipmap(integer target)
    if xglGenerateMipmap=0 then
        xglGenerateMipmap = link_glext_proc("glGenerateMipmap",{GLenum})
    end if
    c_proc(xglGenerateMipmap,{target})
end procedure

global function glGenLists(integer range)
    return c_func(xglGenLists,{range})
end function

integer xglGenVertexArrays = 0

--global function glGenVertexArrays()
global function glCreateVertexArray()
    if xglGenVertexArrays=0 then
        xglGenVertexArrays = link_glext_proc("glGenVertexArrays",{GLuint,C_PTR})
--void glGenVertexArrays( GLsizei n,
--      GLuint *arrays);
    end if
    c_proc(xglGenVertexArrays,{1,pWord})
    atom res = peek4u(pWord) -- always int32, I think...
    return res
end function

--/*
C++ / OpenGL (Desktop)          WebGL 2 Equivalent          WebGL 1 Equivalent (via Extension)
glGenVertexArrays(1, &vao);     gl.createVertexArray()      ext.createVertexArrayOES()
glBindVertexArray(vao);         gl.bindVertexArray(vao);    ext.bindVertexArrayOES(vao);
glDeleteVertexArrays(1, &vao);  gl.deleteVertexArray(vao);
--*/

--now glCreateTexture():
--global function glGenTextures(integer n=1)
--  atom pMem = allocate(4*n)
--  c_proc(xglGenTextures,{n,pMem})
--  integer e = glGetError()
--  if e!=GL_NO_ERROR then ?9/0 end if
--  sequence textures = peek4u({pMem,n})
--  free(pMem)
--  return textures
--end function

integer xglGetAttribLocation = 0

global function glGetAttribLocation(integer program, string name)
    if xglGetAttribLocation=0 then
        xglGetAttribLocation = link_glext_func("glGetAttribLocation",{GLuint,C_PTR},GLint)
    end if
    integer res = c_func(xglGetAttribLocation,{program,name})
    return res
end function

global procedure glGetBooleanv(integer pname, atom pMem)
    c_proc(xglGetBooleanv,{pname,pMem})
end procedure

global procedure glGetDoublev(integer pname, atom pMem)
    c_proc(xglGetDoublev,{pname,pMem})
end procedure

global procedure glGetFloatv(integer pname, atom pMem)
    c_proc(xglGetFloatv,{pname,pMem})
end procedure

global procedure glGetIntegerv(integer pname, atom pMem)
    c_proc(xglGetIntegerv,{pname,pMem})
end procedure

integer xglGetProgramiv = 0

global function glGetProgramParameter(integer shader, pname, dflt=0)
    if xglGetProgramiv=0 then
        xglGetProgramiv = link_glext_proc("glGetProgramiv",{GLuint,GLenum,C_PTR})
    end if
    poken(pWord,dflt)
    c_proc(xglGetProgramiv,{shader,pname,pWord})
    integer res = peekns(pWord)
    return res
end function

integer xglGetShaderiv = 0

global function glGetShaderParameter(integer shader, pname, dflt=0)
    if xglGetShaderiv=0 then
        xglGetShaderiv = link_glext_proc("glGetShaderiv",{GLuint,GLenum,C_PTR})
    end if
    poken(pWord,dflt)
    c_proc(xglGetShaderiv,{shader,pname,pWord})
    integer res = peekns(pWord)
    return res
end function

integer xglGetProgramInfoLog = 0,
        logBufferLength = 128
atom pLogBuffer = allocate(logBufferLength)

procedure expand_buffer(integer len)
    if len>logBufferLength then
        pLogBuffer = free(pLogBuffer)
        while len>logBufferLength do logBufferLength*=2 end while
        pLogBuffer = allocate(logBufferLength)
    end if
end procedure

global function glGetProgramInfoLog(integer program)
    if xglGetProgramInfoLog=0 then
        xglGetProgramInfoLog = link_glext_proc("glGetProgramInfoLog",{GLuint,GLsizei,C_PTR,C_PTR})
    end if
    expand_buffer(glGetProgramParameter(program, GL_INFO_LOG_LENGTH))
    c_proc(xglGetProgramInfoLog,{program,logBufferLength,pWord,pLogBuffer})
    integer len = peekns(pWord)
    string res = peek_string(pLogBuffer)
    if length(res)!=len then ?9/0 end if
    return res
end function

integer xglGetShaderInfoLog = 0

global function glGetShaderInfoLog(integer shader)
    if xglGetShaderInfoLog=0 then
        xglGetShaderInfoLog = link_glext_proc("glGetShaderInfoLog",{GLuint,GLsizei,C_PTR,C_PTR})
    end if
    expand_buffer(glGetShaderParameter(shader, GL_INFO_LOG_LENGTH))
    c_proc(xglGetShaderInfoLog,{shader,logBufferLength,pWord,pLogBuffer})
    integer len = peekns(pWord)
    string res = peek_string(pLogBuffer)
    if length(res)!=len then ?9/0 end if
    return res
end function

global function glGetString(integer name)
    atom ret = c_func(xglGetString, {name})
--untried:
--  return iff(ret?peek_string(ret):"")
    if ret then
--DEV return peek_string(ret)
        integer len = 0
        while peek(ret+len) do len += 1 end while
        return peek({ret, len})
    else
        return ""
    end if
end function

integer xglGetUniformLocation = 0

global function glGetUniformLocation(integer prog, string name)
    if xglGetUniformLocation=0 then
        xglGetUniformLocation = link_glext_func("glGetUniformLocation",{GLuint,C_PTR},GLint)
    end if
    integer res = c_func(xglGetUniformLocation,{prog,name})
    if res=-1 then ?9/0 end if
    return res
end function

object supported_extensions = NULL

global function glSupportsExtension(sequence ext)
    if supported_extensions=NULL then
        supported_extensions = split(glGetString(GL_EXTENSIONS))
    end if
    return find(ext,supported_extensions)!=0
end function

atom xglFogCoordfEXT,
     xglFogCoordfvEXT,
     xglFogCoorddEXT
--   xglFogCoorddvEXT

global procedure enable_GL_EXT_fog_coord()
    xglFogCoordfEXT = link_glext_proc("glFogCoordfEXT",{GLfloat})
    xglFogCoordfvEXT = link_glext_proc("glFogCoordfvEXT",{C_PTR})
    xglFogCoorddEXT = link_glext_proc("glFogCoorddEXT",{GLdouble})
--  xglFogCoorddvEXT = link_glext_proc("glFogCoorddvEXT",{C_PTR})
end procedure

global procedure glFogCoordfEXT(atom a)
--?9/0
--  poke4(call_by_ptr__num_params,1)
--  poke(call_by_ptr_params,atom_to_float32(a))
--  poke4(call_by_ptr__func_addr,GlFogCoordfEXT)
--  call(call_by_ptr_code)
    c_proc(xglFogCoordfEXT,{a})
end procedure

global procedure glFogCoordfvEXT(sequence /*a*/)
?9/0
--  gl_pokef32(gl_vector_buffer,a)
--  poke4(call_by_ptr__num_params,1)
--  poke4(call_by_ptr_params,gl_vector_buffer)
--  poke4(call_by_ptr__func_addr,glFogCoordfvEXT)
--  call(call_by_ptr_code)
--  c_proc(xglFogCoordfvEXT,{gl_vector_buffer})
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
--  c_proc(xglFogCoorddEXT,{??})
end procedure


global procedure glHint(integer target,integer mode)
    c_proc(xglHint,{target,mode})
end procedure

global procedure glIndexi(integer idx)
    c_proc(xglIndexi,{idx})
end procedure

global procedure glLight(integer light, integer pname, object params)
    if atom(params) then
        c_proc(xglLightf,{light,pname,params})
    else
        gl_pokef32(gl_vector_buffer,params)
        c_proc(xglLightfv,{light,pname,gl_vector_buffer})
    end if
end procedure

--global procedure glLightf(integer light,integer pname,atom val)
--  c_proc(xglLightf,{light,pname,val})
--end procedure
--
--global procedure glLightfv(integer light,integer pname,sequence vals)
--  gl_pokef32(gl_vector_buffer,vals)
--  c_proc(xglLightfv,{light,pname,gl_vector_buffer})
--end procedure

global procedure glLightModelf(integer i,atom val)
    c_proc(xglLightModelf,{i,val})
end procedure

global procedure glLightModelfv(integer i,sequence vals)
    gl_pokef32(gl_vector_buffer,vals)
    c_proc(xglLightModelfv,{i,gl_vector_buffer})
end procedure

integer xglLinkProgram = 0

global procedure glLinkProgram(integer prog)
    if xglLinkProgram=0 then
        xglLinkProgram = link_glext_proc("glLinkProgram",{GLuint})
    end if
    c_proc(xglLinkProgram,{prog})
end procedure

global procedure glListBase(atom base)
    c_proc(xglListBase,{base})
end procedure

global procedure glLoadIdentity()
    c_proc(xglLoadIdentity,{})
end procedure

global procedure glLoadMatrixd(atom m)
    c_proc(xglLoadMatrixd,{m})
end procedure

global procedure glMaterial(integer face, pname, object v)
    if pname=GL_SHININESS then
        if not atom(v) then
            assert(length(v)==1)
            v = v[1]
            assert(atom(v))
        end if
        c_proc(xglMaterialf,{face,pname,v})
    else
        assert(sequence(v))
        gl_pokef32(gl_vector_buffer,v)
        c_proc(xglMaterialfv,{face,pname,gl_vector_buffer})
    end if
end procedure

global procedure glMatrixMode(integer mode)
    c_proc(xglMatrixMode,{mode})
end procedure

global procedure glNewList(integer list,integer mode)
    c_proc(xglNewList,{list,mode})
end procedure

global procedure glNormal(atom nx, atom ny, atom nz)
    c_proc(xglNormal3d,{nx,ny,nz})
end procedure

global procedure glNormal3(sequence s)
    c_proc(xglNormal3d,s)
end procedure

--global procedure glNormal3b(sequence normal)
--  c_proc(xglNormal3b,normal)
--end procedure

--global procedure glNormal3d(atom nx, atom ny, atom nz)
--  c_proc(xglNormal3d,{nx,ny,nz})
--end procedure

--global procedure glNormal3f(atom nx, atom ny, atom nz)
--  c_proc(xglNormal3f,{nx,ny,nz})
--end procedure

--global procedure glNormal3i(sequence normal)
--  c_proc(xglNormal3i,normal)
--end procedure
--
--global procedure glNormal3s(sequence normal)
--  c_proc(xglNormal3s,normal)
--end procedure

global procedure glOrtho(atom left, right, top, bottom, zNear, zFar)
    c_proc(xglOrtho,{left,right,top,bottom,zNear,zFar})
end procedure

global procedure glPixelStorei(integer pname,integer param)
    c_proc(xglPixelStorei,{pname,param})
end procedure

global procedure glPointSize(atom size)
    c_proc(xglPointSize,{size})
end procedure

global procedure glPolygonMode(integer face,integer mode)
    c_proc(xglPolygonMode,{face,mode})
end procedure

global procedure glPopMatrix()
    c_proc(xglPopMatrix,{})
end procedure

global procedure glPushMatrix()
    c_proc(xglPushMatrix,{})
end procedure

global procedure glRasterPos2i(sequence pos)
    c_proc(xglRasterPos2i,pos)
end procedure

global procedure glRectf(sequence rect)
    c_proc(xglRectf,rect)
end procedure

global procedure glRotate(atom angle, x, y, z)
    c_proc(xglRotated,{angle,x,y,z})
end procedure

--DEV kill*2?
global procedure glRotated(atom angle, x, y, z)
    c_proc(xglRotated,{angle,x,y,z})
end procedure

global procedure glRotatef(atom angle, x, y, z)
    c_proc(xglRotatef,{angle,x,y,z})
end procedure

global procedure glScale(sequence scale)
    c_proc(xglScaled,scale)
end procedure

--DEV kill*2?
global procedure glScaled(sequence scale)
    c_proc(xglScaled,scale)
end procedure

global procedure glScalef(sequence scale)
    c_proc(xglScalef,scale)
end procedure

global procedure glScissor(integer x, y, width, height)
    c_proc(xglScissor, {x, y, width, height})
end procedure

global procedure glShadeModel(integer model)
    c_proc(xglShadeModel,{model})
end procedure

local function tg_raw_string_ptr(string s)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s) but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in existence.
--       In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
--       (one example in theGUI where that /still/ applies would be in setting say tvItem.pszText)
--
    atom res
    #ilASM{
        [32]
            mov eax,[s]
            lea edi,[res]
            shl eax,2
        [64]
            mov rax,[s]
            lea rdi,[res]
            shl rax,2
        []
            call :%pStoreMint
          }
    return res
end function

--global function iup_string_pointer_array(sequence strings)
local function XX_string_pointer_array(sequence strings)
    integer W = machine_word()
    atom pArray = allocate(length(strings)*W,true),
         ptr = pArray
    for i=1 to length(strings) do
        string si = strings[i]
--      pokeN(ptr,IupRawStringPtr(si),W)
        pokeN(ptr,tg_raw_string_ptr(si),W)
        ptr += W
    end for
    return pArray
end function

integer xglShaderSource = 0

global procedure glShaderSource(integer shader, string source)
    if xglShaderSource=0 then
        xglShaderSource = link_glext_proc("glShaderSource",{GLuint,GLsizei,C_PTR,C_PTR})
    end if
--  integer count = length(strings)
--  atom pStrings = iup_string_pointer_array(strings)   -- (automatically freed)
--  c_proc(xglShaderSource,{shader,count,pStrings,NULL})
--  atom pShader = iup_string_pointer_array({source})
    atom pShader = XX_string_pointer_array({source})
    c_proc(xglShaderSource,{shader,1,pShader,NULL})
end procedure

local function compile_shader(string src, integer sourcetype) -- crash on failure
    integer shader = glCreateShader(sourcetype)
    glShaderSource(shader,src)
    glCompileShader(shader)
    integer compiled = glGetShaderParameter(shader, GL_COMPILE_STATUS)
    if not compiled then crash(glGetShaderInfoLog(shader)) end if
--?glGetShaderInfoLog(shader) -- ("" means "all good" here)
    return shader
end function

global function glCompileProgram(string vertex_shader, fragment_shader) -- ""
    integer vs = compile_shader(vertex_shader, GL_VERTEX_SHADER),
            fs = compile_shader(fragment_shader, GL_FRAGMENT_SHADER),
            id = glCreateProgram()
    glAttachShader(id,vs)
    glAttachShader(id,fs)
    glLinkProgram(id)
    integer linked = glGetProgramParameter(id, GL_LINK_STATUS)
    if not linked then crash(glGetProgramInfoLog(id)) end if
    vs = glDeleteShader(vs)
    fs = glDeleteShader(fs)
    return id
end function

global function glSimpleA7texcoords(integer faces)
    -- (nb expressly ask flatten for a dword-sequence, not a string)
    return flatten(repeat({1,0, 1,1, 0,0,  1,1, 0,1, 0,0},faces),{})    -- (desktop)
--  return flatten(repeat({1,1, 1,0, 0,1,  1,0, 0,0, 0,1},faces),{})    -- (browser)
end function

global procedure glTexCoord(atom s, t=0, r=0, q=1)
    c_proc(xglTexCoord4d,{s,t,r,q})
end procedure

global procedure glTexImage2D(integer target, level, components, width, height, border, fmt, datatype, atom pTexture)
    c_proc(xglTexImage2D,{target,level,components,width,height,border,fmt,datatype,pTexture})
end procedure

--/*
global procedure glTexImage2Dc(integer target, level, components, width, height, border, fmt, datatype, cdCanvas cd_canvas)
    --
    -- There is probably a better way than this, but at least it
    -- is guaranteed to work and is only a one-off setup overhead.
    -- Note this may well create the texture upside down and back
    -- to front in pure OpenGL terms, but since we can easily fix
    -- all that with texture co-ordinates we simply don't care.
    --
    sequence {r,g,b} = cdCanvasGetImageRGB(cd_canvas, 0, 0, width, height)
    atom pTexture = allocate(width*height*4),
         pWork = pTexture
    for i=1 to width*height do
        poke4(pWork,rgb(b[i],g[i],r[i]))
        pWork += 4
    end for
    c_proc(xglTexImage2D,{target,level,components,width,height,border,fmt,datatype,pTexture})
    free(pTexture)
end procedure
--*/

global procedure glTexParameteri(integer target, pname, param)
    c_proc(xglTexParameteri,{target,pname,param})
end procedure

global procedure glTexEnvi(integer i, j, k)
    c_proc(xglTexEnvi,{i,j,k})
end procedure

global procedure glTexEnvf(integer i, j, atom k)
    c_proc(xglTexEnvf,{i,j,k})
end procedure

global procedure glTexGeni(integer coord, pname, param)
    c_proc(xglTexGeni,{coord,pname,param})
end procedure

global procedure glTranslate(atom x, y, z)
    c_proc(xglTranslated,{x,y,z})
end procedure

global procedure glTranslated(atom x, y, z)
    c_proc(xglTranslated,{x,y,z})
end procedure

global procedure glTranslatef(atom x, y, z)
    c_proc(xglTranslatef,{x,y,z})
end procedure

integer xglUniform1f = 0

global procedure glUniform1f(integer location, atom v0)
    if xglUniform1f=0 then
        xglUniform1f = link_glext_proc("glUniform1f",{GLint,GLfloat})
--      xglUniform1f = link_glext_proc("glUniform1f",{GLint,C_FLOAT})
--      xglUniform1f = link_glext_proc("glUniform1f",{GLint,C_DOUBLE})
--      xglUniform1f = link_glext_proc("glUniform1f",{GLint,C_INT})
    end if
    c_proc(xglUniform1f,{location,v0})
end procedure

integer xglUniform2f = 0

global procedure glUniform2f(integer location, atom v0, v1)
    if xglUniform2f=0 then
        xglUniform2f = link_glext_proc("glUniform2f",{GLint,GLfloat,GLfloat})
    end if
    c_proc(xglUniform2f,{location,v0,v1})
end procedure

integer xglUniform3f = 0

global procedure glUniform3f(integer location, atom v0, v1, v2)
    if xglUniform3f=0 then
        xglUniform3f = link_glext_proc("glUniform3f",{GLint,GLfloat,GLfloat,GLfloat})
    end if
    c_proc(xglUniform3f,{location,v0,v1,v2})
end procedure

integer xglUniform4f = 0

global procedure glUniform4f(integer location, atom v0, v1, v2, v3)
    if xglUniform4f=0 then
        xglUniform4f = link_glext_proc("glUniform4f",{GLint,GLfloat,GLfloat,GLfloat,GLfloat})
    end if
    c_proc(xglUniform4f,{location,v0,v1,v2,v3})
end procedure

integer xglUniform1i = 0

global procedure glUniform1i(integer location, integer v0)
    if xglUniform1i=0 then
        xglUniform1i = link_glext_proc("glUniform1i",{GLint,GLint})
--      xglUniform1i = link_glext_proc("glUniform1i",{GLint,GLfloat})
--      xglUniform1i = link_glext_proc("glUniform1i",{GLint,C_FLOAT})
--      xglUniform1i = link_glext_proc("glUniform1i",{GLint,C_DOUBLE})
--      xglUniform1i = link_glext_proc("glUniform1i",{GLint,C_INT})
    end if
    c_proc(xglUniform1i,{location,v0})
end procedure

integer xglUniformMatrix4fv = 0

global procedure glUniformMatrix4fv(integer location, object count, transpose=NULL, pData=NULL)
    if pData=NULL then
        if transpose!=NULL then
            assert(count==false)
            pData = transpose
        else
            pData = count
        end if
    else
        assert(count==1)
        assert(transpose==false)
    end if
    if sequence(pData) then pData = glFloat32Array(pData) end if
    if xglUniformMatrix4fv=0 then
        xglUniformMatrix4fv = link_glext_proc("glUniformMatrix4fv",{GLint,GLsizei,C_INT,C_PTR})
    end if
    c_proc(xglUniformMatrix4fv,{location,1,false,pData})
end procedure

integer xglUseProgram = 0

global procedure glUseProgram(integer prog)
    if xglUseProgram=0 then
        xglUseProgram = link_glext_proc("glUseProgram",{GLuint})
    end if
    c_proc(xglUseProgram,{prog})
end procedure

global procedure glVertex(atom x, y, z=0, w=1)
    c_proc(xglVertex4d,{x,y,z,w})
end procedure

--/*
global procedure glVertex2d(sequence vertex)
    c_proc(xglVertex2d,vertex)
end procedure

global procedure glVertex2f(sequence vertex)
    c_proc(xglVertex2f,vertex)
end procedure

global procedure glVertex2i(sequence vertex)
    c_proc(xglVertex2i,vertex)
end procedure

global procedure glVertex2s(sequence vertex)
    c_proc(xglVertex2s,vertex)
end procedure
--*/

global procedure glVertex3d(sequence vertex)
    c_proc(xglVertex3d,vertex)
end procedure

--/*
global procedure glVertex3f(sequence vertex)
    c_proc(xglVertex3f,vertex)
end procedure

global procedure glVertex3i(sequence vertex)
    c_proc(xglVertex3i,vertex)
end procedure

global procedure glVertex3s(sequence vertex)
    c_proc(xglVertex3s,vertex)
end procedure

global procedure glVertex4d(sequence vertex)
    c_proc(xglVertex4d,vertex)
end procedure

global procedure glVertex4f(sequence vertex)
    c_proc(xglVertex4f,vertex)
end procedure

global procedure glVertex4i(sequence vertex)
    c_proc(xglVertex4i,vertex)
end procedure

global procedure glVertex4s(sequence vertex)
    c_proc(xglVertex4s,vertex)
end procedure
--*/

--global procedure glVertex3dv(sequence vertex)
--  c_proc(xglVertex3d,vertex)
--end procedure

integer xglVertexAttribPointer = 0

global procedure glVertexAttribPointer(integer index, size, datatype, normalized, stride, atom pVertices)
    if xglVertexAttribPointer=0 then
        xglVertexAttribPointer = link_glext_proc("glVertexAttribPointer",{GLuint,GLint,GLenum,C_INT,GLsizei,C_PTR})
    end if
    c_proc(xglVertexAttribPointer,{index,size, datatype, normalized, stride, pVertices})
end procedure

global procedure glVertexPointer(integer size, ptype, stride, atom pointer)
    c_proc(xglVertexPointer, {size, ptype, stride, pointer})
end procedure

global procedure glViewport(integer x, y, w, h)
    c_proc(xglViewport,{x,y,w,h})
end procedure

integer xwglGetCurrentContext = 0

global procedure wglMakeCurrent(atom device_context, rendering_context=NULL)
--glXMakeCurrent?
    if rendering_context=NULL and device_context!=NULL then
        if xwglGetCurrentContext=0 then
            xwglGetCurrentContext = define_c_func(opengl32,"wglGetCurrentContext",{},C_PTR)
--HGLRC wglGetCurrentContext();
        end if
        rendering_context = c_func(xwglGetCurrentContext,{})
    end if
    bool res = c_func(xwglMakeCurrent,{device_context,rendering_context})
    assert(res)
end procedure

global procedure wglDeleteContext(atom hdc)
--glXDestroyContext?
    bool res = c_func(xwglDeleteContext,{hdc})
    assert(res)
end procedure

--/*
// 1. Choose a pixel format that supports PFD_DRAW_TO_PBUFFER
int pfdAttribs[] = {
    PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER | PFD_DRAW_TO_PBUFFER,
    PFD_TYPE_RGBA, 32,          // colour bits
    24,                         // depth bits
    0,                          // accumulation
    8,                          // stencil
    PFD_MAIN_PLANE,
    0,0,0,0
};
int pf = ChoosePixelFormat(hDC, &pfd);
SetPixelFormat(hDC, pf, &pfd);

// 2. Create the pbuffer
int pbufAttribs[] = {
    WGL_PBUFFER_LARGEST_ARB, FALSE,
    WGL_WIDTH,  width,
    WGL_HEIGHT, height,
    WGL_TEXTURE_FORMAT_ARB, WGL_TEXTURE_RGBA_ARB,
    WGL_TEXTURE_TARGET_ARB, WGL_TEXTURE_2D_ARB,
    0
};
HPBUFFERARB hPbuffer = wglCreatePbufferARB(hDC, pf, pbufAttribs);
HPBUFFERARB hOldPbuffer = wglGetCurrentPbufferARB();
HDC hPbufferDC = wglGetPbufferDCARB(hPbuffer);

// 3. Make a GL context current on the pbuffer
HGLRC hRC = wglCreateContext(hPbufferDC);
wglMakeCurrent(hPbufferDC, hRC);

// … render as usual …

// 4. Either read back or bind as texture
glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,sysMemBuffer);
// OR
glBindTexture(GL_TEXTURE_2D, texName);
glCopyTexSubImage2D(GL_TEXTURE_2D,0,0,0,0,0,width,height);

// 5. Cleanup
wglMakeCurrent(NULL,NULL);
wglDeleteContext(hRC);
wglReleasePbufferDCARB(hPbuffer, hPbufferDC);
wglDestroyPbufferARB(hPbuffer);
--*/

--/*
--c:\windows\syswow64\OPENGL32.DLL
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
(glAttachShader)
glBegin
(glBindAttribLocation)
glBindTexture
glBitmap
glBlendFunc
(glBufferData)
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
(glCompileShader)
glCopyPixels
glCopyTexImage1D
glCopyTexImage2D
glCopyTexSubImage1D
glCopyTexSubImage2D
(glCreateProgram)
(glCreateShader)
glCullFace
glDebugEntry
glDeleteLists
(glDeleteProgram)
(glDeleteShader)
glDeleteTextures
glDepthFunc
glDepthMask
glDepthRange
glDisable
glDisableClientState
(glDisableVertexAttribArray)
glDrawArrays
glDrawBuffer
glDrawElements
glDrawPixels
glEdgeFlag
glEdgeFlagPointer
glEdgeFlagv
glEnable
glEnableClientState
(glEnableVertexAttribArray)
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
(glGenerateMipmap)
glGenLists
glGenTextures (as glCreateTexture)
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
(glGetProgramInfoLog)
(glGetProgramiv) (as glGetProgramParameter)
(glGetShaderInfoLog)
(glGetShaderiv) (as glGetShaderParameter)
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
(glGetUniformLocation)
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
(glLinkProgram)
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
(glShaderSource)
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
(glUseProgram)
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
(glVertexAttribPointer)
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
