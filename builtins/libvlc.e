--
-- builtins/libvlc.e
--
-- DEV as yet undocumented, or even in 7z
-- Requires vlc to be independently installed (32|64 or both: if the "other" dll
-- exists it will/may (automatically) perform requires(32|64) to relaunch itself)
--
requires(WINDOWS) -- nb linux completely untested, aka `/usr/lib/libvlc.so` assumed.
bool bVLC_init = false
integer x_libvlc_get_version,
        x_libvlc_media_get_duration,
        x_libvlc_media_new_path,
        x_libvlc_media_parse,
--      x_libvlc_media_parse_request,
--      x_libvlc_media_parse_with_options,
        x_libvlc_media_player_get_time,
        x_libvlc_media_player_is_playing,
        x_libvlc_media_player_new_from_media,
        x_libvlc_media_player_play,
        x_libvlc_media_player_release,
        x_libvlc_media_player_stop,
        x_libvlc_media_release,
        x_libvlc_new,
        x_libvlc_release
        
--global constant libvlc_media_parse_local = 1

local procedure libvlc_init(bool bRequires3264, bQuiet)
    -- the main(/once-only) part of libvlc_new()
    atom libvlc = NULL
    if platform()=WINDOWS then
        integer m = machine_bits()
        sequence path = {getenv(`SystemDrive`),
                         `Program Files`&iff(m=32?` (x86)`:``),
                         `VideoLAN\VLC`}
        string vlcdir = join_path(path),
               vlcdll = "libvlc.dll",
               origcd = current_dir()
        bool de = get_file_type(vlcdir)=FILETYPE_DIRECTORY
        if de then -- (directory exists)
            assert(chdir(vlcdir))
            libvlc = open_dll(vlcdll,false)
            assert(chdir(origcd))
        end if
        if libvlc=NULL then
            if bRequires3264 and not de then
                -- switch to 32/64 bit phix to match the installed VLC player:
                -- on 32 bit: if Program Files (x86)\VideoLAN\VLC does /NOT/ exist,
                --       and/but Program Files\VideoLAN\VLC /does/ => requires(64)
                -- on 64 bit: if Program Files\VideoLAN\VLC does /NOT/ exist, and/
                --     but Program Files (x86)\VideoLAN\VLC /does/ => requires(32)
                -- intent: signal a bad install, and just don't risk a loopy-loop
                --         (as well of course as it succeeding whenever possible).
                path[2] = iff(m=32?`Program Files`:`Program Files (x86)`)
                vlcdir = join_path(path)
                if get_file_type(vlcdir)=FILETYPE_DIRECTORY then
                    requires(96-m,bQuiet) -- (32<=>64)
                end if
            end if
            crash("cannot open %s",{join_path({vlcdir,vlcdll})})
        end if
    elsif platform()=LINUX then
        -- nb untested by me...
        -- equivalent requires(32|64) code left as an exercise
        libvlc = open_dll(`/usr/lib/libvlc.so`)
        assert(libvlc,`cannot open libvlc.so`)
    else
        ?9/0
    end if
    x_libvlc_get_version = define_c_func(libvlc, "libvlc_get_version",
        {},         --  (void)
        C_PTR)      -- char*
--  -- may be needed below...
--  string version = peek_string(c_func(x_libvlc_get_version, {}))
    x_libvlc_media_get_duration = define_c_func(libvlc,"libvlc_media_get_duration",
        {C_PTR},    --  libvlc_media_t* p_md
        C_INT)      -- libvlc_time_t (int64)
--      -- aside: result is really int64, but using an int32 (on 32-bit, that is)
--      --        ie: ?elapsed(#7FFFFFFFF/1000) -- over a year, so we're good!
--      C_INT64)    -- libvlc_time_t (int64) -- (DEV/ugh crashes on 32bit...)
--      C_DOUBLE)   -- libvlc_time_t (int64) -- (clutching at straws?)
    -- nb not as documented, but it seems to work...
    x_libvlc_media_new_path = define_c_func(libvlc,"libvlc_media_new_path",
        {C_PTR,     --  libvlc_instance_t*
         C_PTR},    --  const char *path
--      {C_PTR},    --  const char *path -- (as docs... may be a 4.0+ thing)
        C_PTR)      -- libvlc_media_t*
    x_libvlc_media_parse = define_c_func(libvlc,"libvlc_media_parse",
        {C_PTR},    --  libvlc_media_t* p_md
        C_INT)      -- int
--  -- nb not present in 3.0.21.0:
--  x_libvlc_media_parse_request = define_c_func(libvlc,"libvlc_media_parse_request",
--      {C_PTR,     --  libvlc_instance_t* inst
--       C_PTR,     --  libvlc_media_t* p_md
--       C_INT,     --  libvlc_media_parse_flag_t parse_flag
--       C_INT},    --  int timeout
--      C_INT),     -- int
--  -- DEV couldn't get this to work...	
--  x_libvlc_media_parse_with_options = define_c_func(libvlc,"libvlc_media_parse_with_options",
--      {C_PTR,     --  libvlc_media_t* p_md
--       C_INT,     --  libvlc_media_parse_flag_t parse_flag
--       C_INT},    --  int timeout
--      C_INT),     -- int
    x_libvlc_media_player_get_time = define_c_func(libvlc,"libvlc_media_player_get_time",
        {C_PTR},    --  libvlc_media_player_t* p_mi
        C_INT)      -- libvlc_time_t (int64, as per libvlc_media_get_duration)
    x_libvlc_media_player_is_playing = define_c_func(libvlc,"libvlc_media_player_is_playing",
        {C_PTR},    --  libvlc_media_player_t* p_mi
        C_BOOL)     -- bool
    x_libvlc_media_player_new_from_media = define_c_func(libvlc,"libvlc_media_player_new_from_media",
        {C_PTR,     --  libvlc_instance_t *inst
         C_PTR},    --  libvlc_media_t *p_md
        C_PTR)      -- libvlc_media_player_t*
    x_libvlc_media_player_play = define_c_func(libvlc,"libvlc_media_player_play",
        {C_PTR},    --  libvlc_media_player_t* p_mi
        C_INT)      -- int
    x_libvlc_media_player_release = define_c_proc(libvlc,"libvlc_media_player_release",
        {C_PTR})    --  libvlc_media_player_t* p_mi
    x_libvlc_media_player_stop = define_c_proc(libvlc,"libvlc_media_player_stop",
        {C_PTR})    --  libvlc_media_player_t* p_mi
    x_libvlc_media_release = define_c_proc(libvlc,"libvlc_media_release",
        {C_PTR})    --  libvlc_media_t* p_md
    x_libvlc_new = define_c_func(libvlc,"libvlc_new",
        {C_INT,     --  int argc (0)
         C_PTR},    --  const char* const* argv (NULL)
        C_PTR)      -- libvlc_instance_t*
    x_libvlc_release = define_c_proc(libvlc,"libvlc_release",
        {C_PTR})    -- libvlc_instance_t* p_instance
    bVLC_init = true
end procedure

global function libvlc_get_version()
    if not bVLC_init then libvlc_init(true,false) end if
    -- nb only tested thus far on 3.0.21.0 (win 32/64)
    return peek_string(c_func(x_libvlc_get_version, {}))
end function

global function libvlc_media_get_duration(atom media)
    -- nb media must be parsed(/played) before this call
    atom ms = c_func(x_libvlc_media_get_duration,{media})
    return ms -- (-1 on error, aka -0.001s)
end function

global function libvlc_media_new_path(atom vlc, string path)
    atom media = c_func(x_libvlc_media_new_path,{vlc,path})
    return media
end function

global function libvlc_media_parse(atom media)
    integer res = c_func(x_libvlc_media_parse,{media})
    return res
end function

global function libvlc_media_player_get_time(atom player)
    atom ms = c_func(x_libvlc_media_player_get_time,{player})
    return ms -- (-1 on error, aka -0.001s)
end function

global function libvlc_media_player_is_playing(atom player)
    bool res = c_func(x_libvlc_media_player_is_playing,{player})
    return res
end function

global function libvlc_media_player_new_from_media(atom vlc, media)
    atom player = c_func(x_libvlc_media_player_new_from_media,{vlc,media})
    return player
end function

global function libvlc_media_player_play(atom player)
    integer res = c_func(x_libvlc_media_player_play,{player})
    return res -- 0:OK, -1: error
end function

global function libvlc_media_player_release(atom player)
    if player then
        c_proc(x_libvlc_media_player_release,{player})
    end if
    return NULL -- (nullify your player variable!)
end function

global procedure libvlc_media_player_stop(atom player)
    c_proc(x_libvlc_media_player_stop,{player})
end procedure

global procedure libvlc_media_release(atom media)
    c_proc(x_libvlc_media_release,{media})
end procedure

global function libvlc_new(bool bRequires3264=true, bQuiet=false)
    if not bVLC_init then libvlc_init(bRequires3264,bQuiet) end if
    -- (Aside: once bVLC_init is set, the two args wouldn't be
    --         obeyed anyway, since the dll/so is already open.)
    -- NB the two args above are /NOT/ the 0,NULL of argc,argv
    --    that you'll find in most C-based examples and below..
    atom vlc = c_func(x_libvlc_new,{0,NULL})
    return vlc
end function

global procedure libvlc_release(atom vlc)
    c_proc(x_libvlc_release,{vlc})
end procedure


--/*
-- default VLC plugin path, can be overridden when VLC library is initialised, see function new( ... ) 
    constant VLC_PLUGIN_PATH = iff(platform()=WINDOWS?`C:\Program Files\VideoLAN\VLC\plugins`
                                                     :"/usr/lib/vlc/plugins")

/* core */

export memstruct libvlc_module_description_t
        pointer char psz_name
        pointer char psz_shortname
        pointer char psz_longname
        pointer char psz_help
        pointer int  p_next           --// struct libvlc_module_description_t *         p_next
end memstruct

--constant libvlc_new_             = define_c_func(libvlc, "libvlc_new", { C_INT, C_POINTER }, C_POINTER)            --// Create and initialize a libvlc instance
--constant libvlc_release_         = define_c_proc(libvlc, "libvlc_release", { C_POINTER })                          --// Decrement the reference count of a libvlc instance, and destroy it if it reaches zero
--constant libvlc_get_version_     = define_c_func(libvlc, "libvlc_get_version", {}, C_POINTER)                      --// Retrieve libvlc version
constant libvlc_get_compiler_      = define_c_func(libvlc, "libvlc_get_compiler", {}, C_POINTER)                     --// Retrieve libvlc compiler version
constant libvlc_get_changeset_     = define_c_func(libvlc, "libvlc_get_changeset", {}, C_POINTER)                    --// Retrieve libvlc changeset
constant libvlc_audio_filter_list_ = define_c_func(libvlc, "libvlc_audio_filter_list_get", { C_POINTER }, C_POINTER) --// Returns a list of audio filters that are available
constant libvlc_video_filter_list_ = define_c_func(libvlc, "libvlc_video_filter_list_get", { C_POINTER }, C_POINTER) --// Returns a list of video filters that are available

/* media */

export memstruct libvlc_media_stats_t
        int     i_read_bytes
        float f_input_bitrate
        int     i_demux_read_bytes
        float f_demux_bitrate
        int     i_demux_corrupted
        int     i_demux_discontinuity
        int     i_decoded_video
        int     i_decoded_audio
        int     i_displayed_pictures
        int     i_lost_pictures
        int     i_played_abuffers
        int     i_lost_abuffers
        int     i_sent_packets
        int     i_sent_bytes
        float f_send_bitrate
end memstruct

--// libvlc_state_t

export enum

        stateNothingSpecial = 0,
        stateOpening,
        stateBuffering,
        statePlaying,
        statePaused,
        stateStopped,
        stateEnded,
        stateError 

export constant MEDIASTATE =
{
        "NothingSpecial",
        "Opening",
        "Buffering",
        "Playing",
        "Paused",
        "Stopped",
        "Ended",
        "Error"
}

--// libmeta_t

export enum

        metaTitle = 0,
        metaArtist,     
        metaGenre,      
        metaCopyright,  
        metaAlbum,      
        metaTrackNumber,        
        metaDescription,        
        metaRating,     
        metaDate,       
        metaSetting,    
        metaURL,        
        metaLanguage,   
        metaNowPlaying, 
        metaPublisher,  
        metaEncodedBy,  
        metaArtworkURL, 
        metaTrackID     

export constant META =
{
        "title",
        "artist",
        "genre",
        "copyright",
        "album",
        "tracknumber",
        "description",
        "rating",
        "date",
        "setting",
        "url",
        "language",
        "nowplaying",
        "publisher",
        "encodedBy",
        "artworurl",
        "trackid"
}

constant libvlc_media_new_location_ = define_c_func(libvlc, "libvlc_media_new_location", { C_POINTER, C_POINTER }, C_POINTER)   --// Create a media with a certain given media resource location, for instance a valid URL 
--constant libvlc_media_new_path_   = define_c_func(libvlc, "libvlc_media_new_path", { C_POINTER, C_POINTER }, C_POINTER)       --// Create a media for a certain file path
--constant libvlc_media_new_fd_     = define_c_func(libvlc, "libvlc_media_new_fd", { C_POINTER, C_INT }, C_POINTER)             --// Create a media for an already open file descriptor (pipe etc ...)
--constant libvlc_media_release_        = define_c_proc(libvlc, "libvlc_media_release", { C_POINTER })                              --// Decrement the reference count of a media descriptor object
constant libvlc_media_add_option_   = define_c_proc(libvlc, "libvlc_media_add_option", { C_POINTER, C_POINTER })                --// Add an option to the media
constant libvlc_media_add_option_flag_ = define_c_proc(libvlc, "libvlc_media_add_option_flag", { C_POINTER, C_POINTER, C_INT }) --// Add an option to the media with configurable flags


--constant libvlc_media_get_duration_ = define_c_func(libvlc, "libvlc_media_get_duration", { C_POINTER }, C_DOUBLE)           --// Get duration (in ms) of media descriptor object item
constant libvlc_media_get_stats_    = define_c_func(libvlc, "libvlc_media_get_stats", { C_POINTER, C_POINTER }, C_INT)                  --// Get the current statistics about the media
constant libvlc_media_get_state_    = define_c_func(libvlc, "libvlc_media_get_state", { C_POINTER }, C_INT)                   --// Get current state of media descriptor object (libvlc_state_t)
constant libvlc_media_get_mrl_      = define_c_func(libvlc, "libvlc_media_get_mrl", { C_POINTER }, C_POINTER)                 --// Get the media resource locator (mrl) from a media descriptor object 

constant libvlc_media_get_meta_     = define_c_func(libvlc, "libvlc_media_get_meta", { C_POINTER, C_INT }, C_POINTER)         --// Read the meta of the media
constant libvlc_media_set_meta_     = define_c_proc(libvlc, "libvlc_media_set_meta", { C_POINTER, C_INT, C_POINTER })         --// Set the meta of the media 
                                                                                                                                                                                                                                                                                                                                                --// (this function will not save the meta, call libvlc_media_save_meta 
                                                                                                                                                                                                                                                                                                                                                --// in order to save the meta)
constant libvlc_media_save_meta_    = define_c_func(libvlc, "libvlc_media_save_meta", { C_POINTER }, C_INT)                   --// Save the meta previously set
constant libvlc_media_parse_        = define_c_proc(libvlc, "libvlc_media_parse", { C_POINTER })                              --// Parse a media

/* media player */

constant libvlc_media_player_new_             = define_c_func( libvlc, "libvlc_media_player_new", { C_POINTER }, C_POINTER)
--constant libvlc_media_player_new_from_media_  = define_c_func( libvlc, "libvlc_media_player_new_from_media", { C_POINTER }, C_POINTER)
--constant libvlc_media_player_release_       = define_c_proc( libvlc, "libvlc_media_player_release", { C_POINTER  })
constant libvlc_media_player_set_media_       = define_c_proc( libvlc, "libvlc_media_player_set_media", { C_POINTER, C_POINTER  })
constant libvlc_media_player_get_media_       = define_c_func( libvlc, "libvlc_media_player_get_media", { C_POINTER }, C_POINTER)
--constant libvlc_media_player_is_playing_    = define_c_func( libvlc, "libvlc_media_player_is_playing", { C_POINTER }, C_INT)
--constant libvlc_media_player_play_              = define_c_func( libvlc, "libvlc_media_player_play", { C_POINTER }, C_INT)
constant libvlc_media_player_set_pause_       = define_c_proc( libvlc, "libvlc_media_player_set_pause", { C_POINTER, C_INT  })
constant libvlc_media_player_pause_           = define_c_proc( libvlc, "libvlc_media_player_pause", { C_POINTER  })
constant libvlc_media_player_stop_            = define_c_proc( libvlc, "libvlc_media_player_stop", { C_POINTER  })
constant libvlc_media_player_set_xwindow_     = define_c_proc( libvlc, "libvlc_media_player_set_xwindow", { C_POINTER, C_POINTER  })
constant libvlc_media_player_get_xwindow_     = define_c_func( libvlc, "libvlc_media_player_get_xwindow", { C_POINTER }, C_INT)
constant libvlc_media_player_set_hwnd_        = define_c_proc( libvlc, "libvlc_media_player_set_hwnd", { C_POINTER, C_POINTER  })
constant libvlc_media_player_get_hwnd_        = define_c_func( libvlc, "libvlc_media_player_get_hwnd", { C_POINTER }, C_POINTER)
constant libvlc_media_player_get_length_      = define_c_func( libvlc, "libvlc_media_player_get_length", { C_POINTER }, C_INT)
--constant libvlc_media_player_get_time_          = define_c_func( libvlc, "libvlc_media_player_get_time", { C_POINTER }, C_INT)
constant libvlc_media_player_get_position_    = define_c_func( libvlc, "libvlc_media_player_get_position", { C_POINTER }, C_FLOAT)
constant libvlc_media_player_set_position_    = define_c_proc( libvlc, "libvlc_media_player_set_position", { C_POINTER, C_FLOAT  })
constant libvlc_media_player_will_play_       = define_c_func( libvlc, "libvlc_media_player_will_play", { C_POINTER }, C_INT)
constant libvlc_media_player_set_title_       = define_c_proc( libvlc, "libvlc_media_player_set_title", { C_POINTER, C_INT  })
constant libvlc_media_player_get_title_       = define_c_func( libvlc, "libvlc_media_player_get_title", { C_POINTER }, C_INT)
constant libvlc_media_player_get_rate_        = define_c_func( libvlc, "libvlc_media_player_get_rate", { C_POINTER }, C_FLOAT)
constant libvlc_media_player_set_rate_        = define_c_func( libvlc, "libvlc_media_player_set_rate", { C_POINTER, C_FLOAT }, C_INT)
constant libvlc_media_player_get_state_       = define_c_func( libvlc, "libvlc_media_player_get_state", { C_POINTER }, C_INT)
constant libvlc_media_player_get_fps_         = define_c_func( libvlc, "libvlc_media_player_get_fps", { C_POINTER }, C_FLOAT)
constant libvlc_media_player_is_seekable_     = define_c_func( libvlc, "libvlc_media_player_is_seekable", { C_POINTER }, C_INT)
constant libvlc_media_player_can_pause_       = define_c_func( libvlc, "libvlc_media_player_can_pause", { C_POINTER }, C_INT)
constant libvlc_toggle_fullscreen_            = define_c_proc( libvlc, "libvlc_toggle_fullscreen", { C_POINTER  })
constant libvlc_set_fullscreen_               = define_c_proc( libvlc, "libvlc_set_fullscreen", { C_POINTER, C_INT  })
constant libvlc_get_fullscreen_               = define_c_func( libvlc, "libvlc_get_fullscreen", { C_POINTER }, C_INT)
constant libvlc_video_set_key_input_          = define_c_proc( libvlc, "libvlc_video_set_key_input", { C_POINTER, C_UINT  })
constant libvlc_video_set_mouse_input_        = define_c_proc( libvlc, "libvlc_video_set_mouse_input", { C_POINTER, C_UINT  })
constant libvlc_video_get_size_               = define_c_func( libvlc, "libvlc_video_get_size", { C_POINTER, C_UINT, C_POINTER, C_POINTER }, C_INT)
constant libvlc_video_get_cursor_             = define_c_func( libvlc, "libvlc_video_get_cursor", { C_POINTER, C_UINT, C_POINTER, C_POINTER }, C_INT)
constant libvlc_video_get_scale_              = define_c_func( libvlc, "libvlc_video_get_scale", { C_POINTER }, C_FLOAT)
constant libvlc_video_set_scale_              = define_c_proc( libvlc, "libvlc_video_set_scale", { C_POINTER, C_FLOAT  })
constant libvlc_video_get_aspect_ratio_       = define_c_func( libvlc, "libvlc_video_get_aspect_ratio", { C_POINTER }, C_POINTER)
constant libvlc_video_set_aspect_ratio_       = define_c_proc( libvlc, "libvlc_video_set_aspect_ratio", { C_POINTER, C_POINTER  })
constant libvlc_audio_output_set_device_type_ = define_c_proc( libvlc, "libvlc_audio_output_set_device_type", { C_POINTER, C_INT  })
constant libvlc_audio_toggle_mute_            = define_c_proc( libvlc, "libvlc_audio_toggle_mute", { C_POINTER  })
constant libvlc_audio_get_mute_               = define_c_func( libvlc, "libvlc_audio_get_mute", { C_POINTER }, C_INT)
constant libvlc_audio_set_mute_               = define_c_proc( libvlc, "libvlc_audio_set_mute", { C_POINTER, C_INT  })
constant libvlc_audio_get_volume_             = define_c_func( libvlc, "libvlc_audio_get_volume", { C_POINTER }, C_INT)
constant libvlc_audio_set_volume_             = define_c_func( libvlc, "libvlc_audio_set_volume", { C_POINTER, C_INT }, C_INT)

--// export functions/procedures

--export function new(sequence argv={}, sequence pluginPath=VLC_PLUGIN_PATH, integer verbose=false)
--  if (verbose) then
--      argv &= { "-vvv" }
--  end if
--  if length(pluginPath) then
--      setenv("VLC_PLUGIN_PATH", pluginPath)
--  end if
--  atom pargv = #0
--  integer argc = length(argv)
--  if argc > 0 then
--      pargv = allocate_string_pointer_array(argv)
--  end if
--  atom vlc   = c_func( libvlc_new_, { length(argv), pargv })
--  assert(vlc, "could not initialise vlc library")
--  if pargv > 0 then
--      free_pointer_array(pargv)
--  end if
--  return vlc
--end function
--
--export procedure release(atom vlc)
--  if vlc > 0 then
--      c_proc(libvlc_release_, { vlc })
--  end if
--end procedure
-- 
--export function version()
--  return peek_string(c_func(libvlc_get_version_, {}))
--end function

export function compiler()
    return peek_string(c_func(libvlc_get_compiler_, {}))
end function

export function changeset()
    return peek_string(c_func(libvlc_get_changeset_, {}))
end function

function filterList(atom vlc, atom rid, sequence listType)
    sequence moduleList = {}
    if vlc > 0 and rid > 0 then
        atom module = c_func( rid, { vlc })
        while (module.libvlc_module_description_t.p_next != NULL) do
            moduleList &= { peek_string(module.libvlc_module_description_t.psz_name) }
            atom modhelp = module.libvlc_module_description_t.psz_help
            if modhelp then
                moduleList[$] = { listType, moduleList[$], peek_string(modhelp) }
            else
                moduleList[$] = { listType, moduleList[$], "" }
            end if
            module = module.libvlc_module_description_t.p_next
        end while
    end if
    return moduleList
end function

export function audioFilters(atom vlc)
    return filterList(vlc, libvlc_audio_filter_list_, "audio")
end function

export function videoFilters(atom vlc, integer display=false)
    return filterList(vlc, libvlc_video_filter_list_, "video")
end function

export function mediaNewLocation(atom vlc, sequence mrl)
    atom result = -1
    if vlc > 0 and length(mrl) then
        atom psz_mrl = allocate_string(mrl)
        result = c_func(libvlc_media_new_location_, { vlc, psz_mrl })
        free(psz_mrl)
    end if
    return result
end function

--export function mediaNewPath(atom vlc, sequence path)
--  atom result = -1 
--  if vlc > 0 and length(path) then
--      atom psz_path = allocate_string(path)
--      result = c_func(libvlc_media_new_path_, { vlc, psz_path })
--      free(psz_path)
--  end if
--  return result
--end function

--DEV note libvlc simply won't understand results from phix's open()!
--export function mediaNewFD(atom vlc, integer fd)
--  atom result = -1 
--  if vlc > 0 and fd > 0 then
--      result = c_func(libvlc_media_new_path_, { vlc, fd })
--  end if
--  return result
--end function

export procedure mediaAddOption(atom media, sequence options)
    if media > 0 and length(options) then
        atom ppsz_options = allocate_string_pointer_array(options)
        c_proc(libvlc_media_add_option_, { media, ppsz_options })
        free_pointer_array(ppsz_options)
    end if
end procedure

export procedure mediaAddOptionFlag(atom media, sequence options, integer i_flags)
    if media > 0 and length(options) then
        atom ppsz_options = allocate_string_pointer_array(options)
        c_proc(libvlc_media_add_option_flag_, { media, ppsz_options, i_flags })
        free_pointer_array(ppsz_options)
    end if
end procedure

--export procedure mediaRelease(atom media)
--  if media > 0 then
--      c_proc(libvlc_media_release_, { media })
--  end if
--end procedure

export function mediaGetmrl(atom media)
    object result = -1
    if media > 0 then
        result = c_func(libvlc_media_get_mrl_, { media })
        if result > 0 then
            result = peek_string(result)
        end if
    end if
    return result
end function

export function mediaGetMeta(atom media, atom e_meta)
    object result = -1
    if media > 0 and e_meta >= 0 then
        result = c_func(libvlc_media_get_meta_, { media, e_meta })
        if result > 0 then
            result = peek_string(result)
        end if
    end if
    return result
end function

export procedure mediaSetMeta(atom media, atom e_meta, sequence v)
    if media > 0 and e_meta >= 0 and length(v) then
        atom psz_value = allocate_string(v)
        c_proc(libvlc_media_set_meta_, { media, e_meta, psz_value })
        free(psz_value)
    end if
end procedure

export function mediaSaveMeta(atom media)
    if media > 0 then
        return c_func(libvlc_media_save_meta_, { media })
    end if
    return -1
end function

export function mediaGetState(atom media)
    if media > 0 then
        return c_func(libvlc_media_get_state_, { media })
    end if
    return -1
end function

export function mediaGetStats(atom media)
    atom p_stats = 0
    if media > 0 then
        p_stats = allocate(sizeof(libvlc_media_stats_t))
        c_func(libvlc_media_get_stats_, { media, p_stats })
    end if
    --// freed by callee
    return p_stats
end function

--export function mediaGetDuration(atom media)
--  if media > 0 then
--      return c_func(libvlc_media_get_duration_, { media })
--  end if
--  return -1
--end function

export procedure mediaParse(atom media)
    if media > 0 then
        c_proc(libvlc_media_parse_, { media })
    end if
end procedure

export function mediaInfo(atom media)
    sequence mInfo = {}
    if media > 0 then
        mediaParse(media)
        atom mStats = mediaGetStats(media)
        mInfo &= {{ "duration", mediaGetDuration(media) }}
        mInfo &= {{ "read_bytes", mStats.libvlc_media_stats_t.i_read_bytes }}
        mInfo &= {{ "input_bitrate", mStats.libvlc_media_stats_t.f_input_bitrate }}
        mInfo &= {{ "demux_read_bytes", mStats.libvlc_media_stats_t.i_demux_read_bytes }}
        mInfo &= {{ "demux_bitrate", mStats.libvlc_media_stats_t.f_demux_bitrate }}
        mInfo &= {{ "demux_corrupted", mStats.libvlc_media_stats_t.i_demux_corrupted }}
        mInfo &= {{ "demux_discontinuity", mStats.libvlc_media_stats_t.i_demux_discontinuity }}
        mInfo &= {{ "decoded_video", mStats.libvlc_media_stats_t.i_decoded_video }}
        mInfo &= {{ "decoded_audio", mStats.libvlc_media_stats_t.i_decoded_audio }}
        mInfo &= {{ "displayed_pictures", mStats.libvlc_media_stats_t.i_displayed_pictures }}
        mInfo &= {{ "lost_pictures", mStats.libvlc_media_stats_t.i_lost_pictures }}
        mInfo &= {{ "played_abuffers", mStats.libvlc_media_stats_t.i_played_abuffers }}
        mInfo &= {{ "lost_abuffers", mStats.libvlc_media_stats_t.i_lost_abuffers }}
        mInfo &= {{ "sent_packets", mStats.libvlc_media_stats_t.i_sent_packets }}
        mInfo &= {{ "sent_bytes", mStats.libvlc_media_stats_t.i_sent_bytes }}
        mInfo &= {{ "send_bitrate", mStats.libvlc_media_stats_t.f_send_bitrate }}
        free(mStats)
        integer mState = mediaGetState(media)
        if mState >= 0 then
            mInfo &= {{ "state", MEDIASTATE[mState + 1] }}
        end if
        object mrl = mediaGetmrl(media)
        if sequence(mrl) then
            mInfo &= {{ "mrl", mrl }}
        end if
        for i = 1 to length(META) do
            object meta = mediaGetMeta(media, i - 1)
            if sequence(meta) then
                mInfo &= {{ META[i], meta }}
            end if
        end for
    end if
    return mInfo
end function

export function mediaPlayerNew(atom vlc)
    if vlc > 0 then
        return c_func(libvlc_media_player_new_, { vlc })
    end if
    return -1
end function

--export function mediaPlayerNewFromMedia(atom media)
--  if media > 0 then
--      return c_func(libvlc_media_player_new_from_media_, { media })
--  end if
--  return -1
--end function

--export procedure mediaPlayerRelease(atom player)
--  if player > 0 then
--      c_proc(libvlc_media_player_release_, { player })
--  end if
--end procedure

export procedure mediaPlayerSetMedia(atom player, atom media)
    if media > 0 and player > 0 then
        c_proc(libvlc_media_player_set_media_, { player, media })
    end if
end procedure

export function mediaPlayerGetMedia(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_media_, { player })
    end if
    return -1
end function

--export function mediaPlayerPlaying(atom player)
--  if player > 0 then
--      return c_func(libvlc_media_player_is_playing_, { player })
--  end if
--  return -1
--end function

--export function mediaPlayerPlay(atom player)
--  if player > 0 then
--      return c_func(libvlc_media_player_play_, { player })
--  end if
--  return -1
--end function

export procedure mediaPlayerSetPause(atom player, integer do_pause)
    if player > 0 then
        c_proc(libvlc_media_player_set_pause_, { player, do_pause })
    end if
end procedure

export procedure mediaPlayerPause(atom player)
    if player > 0 then
        c_proc(libvlc_media_player_pause_, { player })
    end if
end procedure

export procedure mediaPlayerStop(atom player)
    if player > 0 then
        c_proc(libvlc_media_player_stop_, { player })
    end if
end procedure

ifdef not WINDOWS then

export procedure mediaPlayerSetWindow(atom player, atom drawable)
    if player > 0 and drawable > 0 then
        c_proc(libvlc_media_player_set_xwindow_, { player, drawable })
    end if
end procedure

export function mediaPlayerGetWindow(atom player)
    if player > 0  then
        return c_func(libvlc_media_player_get_xwindow_, { player })
    end if
    return -1
end function

elsedef

export procedure mediaPlayerSetWindow(atom player, atom drawable)
    if player > 0 then
        c_proc(libvlc_media_player_set_hwnd_, { player, drawable })
    end if
end procedure

export function mediaPlayerGetWindow(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_hwnd_, { player })
    end if
    return -1
end function

end ifdef

export function mediaPlayerGetLength(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_length_, { player })
    end if
    return -1
end function

--export function mediaPlayerGetTime(atom player)
--  if player > 0 then
--      return c_func(libvlc_media_player_get_time_, { player })
--  end if
--  return -1
--end function

export function mediaPlayerGetPosition(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_position_, { player })
    end if
    return -1
end function

export procedure mediaPlayerSetPosition(atom player, atom f_pos)
    if player > 0 then
        c_proc(libvlc_media_player_set_position_, { player, f_pos })
    end if
end procedure

export function mediaPlayerWillPlay(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_will_play_, { player })
    end if
    return -1
end function

export function mediaPlayerGetRate(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_rate_, { player })
    end if
    return -1
end function

export function mediaPlayerSetRate(atom player, atom rate)
    if player > 0 then
        return c_func(libvlc_media_player_set_rate_, { player, rate })
    end if
    return -1
end function

export function mediaPlayerGetState(atom player)
    --// libvlc_state_t
    if player > 0 then
        return c_func(libvlc_media_player_get_state_, { player })
    end if
    return -1
end function

export function mediaPlayerGetFPS(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_get_fps_, { player })
    end if
    return -1
end function

export function mediaPlayerSeekable(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_is_seekable_, { player })
    end if
    return -1
end function

export function mediaPlayerCanPause(atom player)
    if player > 0 then
        return c_func(libvlc_media_player_can_pause_, { player })
    end if
    return -1
end function

export procedure toggleFullscreen(atom player)
    if player > 0 then
        c_proc(libvlc_toggle_fullscreen_, { player })
    end if
end procedure

export procedure setFullscreen(atom player, integer b_fullscreen)
    if player > 0 then
        c_proc(libvlc_set_fullscreen_, { player, b_fullscreen })
    end if
end procedure

export function getFullscreen(atom player)
    if player > 0 then
        return c_func(libvlc_get_fullscreen_, { player })
    end if
    return -1
end function

export procedure videoSetKeyInput(atom player, integer on)
    if player > 0 then
        c_proc(libvlc_video_set_key_input_, { player, on })
    end if
end procedure

export procedure videoSetMouseInput(atom player, integer on)
    if player > 0 then
        c_proc(libvlc_video_set_mouse_input_, { player, on })
    end if
end procedure

export function videoGetSize(atom player, integer num=0)
    if player > 0 and num >= 0 then
        atom px = allocate(4)
        atom py = allocate(4)
        integer result = c_func(libvlc_video_get_size_, { player, num, px, py })
        integer x = -1, y = -1
        if result != -1 then
            x = peek4u(px)
            y = peek4u(py)
        end if
        free(px)
        free(py)
        return { x, y }
    end if
    return -1
end function

export function videoGetCursor(atom player, integer num=0)
    if player > 0 and num >= 0 then
        atom px = allocate(4)
        atom py = allocate(4)
        integer result = c_func(libvlc_video_get_cursor_, { player, num, px, py })
        integer x = -1, y = -1
        if result != -1 then
            x = peek4s(px)
            y = peek4s(py)
        end if
        free(px)
        free(py)
        return { x, y }
    end if
    return -1
end function

export function videoGetScale(atom player)
    if player > 0 then
        return c_func(libvlc_video_get_scale_, { player })
    end if
    return -1
end function

export procedure videoSetScale(atom player, atom f_factor)
    if player > 0 and f_factor >= 0 then
        c_proc(libvlc_video_set_scale_, { player, f_factor })
    end if
end procedure

export function videoGetAspectRatio(atom player)
    if player > 0 then
        return peek_string(c_func(libvlc_video_get_aspect_ratio_, { player }))
    end if
    return ""
end function

export procedure videoSetAspectRatio(atom player, sequence aspect)
    if player > 0 then
        atom psz_aspect = allocate_string(aspect)
        c_proc(libvlc_video_set_aspect_ratio_, { player, psz_aspect })
        free(psz_aspect)
    end if
end procedure

export procedure audioOutputSetDeviceType(atom player, integer device_type)
    if player > 0 then
        c_proc(libvlc_audio_output_set_device_type_, { player, device_type })
    end if
end procedure

export procedure audioToggleMute(atom player)
    if player > 0 then
        c_proc(libvlc_audio_toggle_mute_, { player })
    end if
end procedure

export function audioGetMute(atom player)
    if player > 0 then
        return c_func(libvlc_audio_get_mute_, { player })
    end if
    return -1
end function

export procedure audioSetMute(atom player, integer status)
        if player > 0 then
                c_proc(libvlc_audio_set_mute_, { player, status })
        end if
end procedure

export function audioGetVolume(atom player)
    if player > 0 then
        return c_func(libvlc_audio_get_volume_, { player })
    end if
    return -1
end function

export function audioSetVolume(atom player, integer i_volume)
    if player > 0 then
        return c_func(libvlc_audio_set_volume_, { player, i_volume })
    end if
    return -1
end function

ifdef __TEST_LIBVLC__ then
--      printf(1, "%-40s = %s\n", { "__MODULENAME__", __MODULENAME__})
--      printf(1, "%-40s = %d\n", { "libvlc", libvlc})
--      printf(1, "%-40s = %d\n", { "libvlc_new_", libvlc_new_})
--      printf(1, "%-40s = %d\n", { "libvlc_release_", libvlc_release_})
--      printf(1, "%-40s = %d\n", { "libvlc_get_version_", libvlc_get_version_})
        printf(1, "%-40s = %d\n", { "libvlc_get_compiler_", libvlc_get_compiler_})
        printf(1, "%-40s = %d\n", { "libvlc_get_changeset_", libvlc_get_changeset_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_filter_list_", libvlc_audio_filter_list_})
        printf(1, "%-40s = %d\n", { "libvlc_video_filter_list_", libvlc_video_filter_list_})
        printf(1, "%-40s = %d\n", { "libvlc_media_new_location_", libvlc_media_new_location_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_new_path_", libvlc_media_new_path_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_release_", libvlc_media_release_})
        printf(1, "%-40s = %d\n", { "libvlc_media_add_option_", libvlc_media_add_option_})
        printf(1, "%-40s = %d\n", { "libvlc_media_add_option_flag_", libvlc_media_add_option_flag_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_get_duration_", libvlc_media_get_duration_})
        printf(1, "%-40s = %d\n", { "libvlc_media_get_stats_", libvlc_media_get_stats_})
        printf(1, "%-40s = %d\n", { "libvlc_media_get_state_", libvlc_media_get_state_})
        printf(1, "%-40s = %d\n", { "libvlc_media_get_mrl_", libvlc_media_get_mrl_})
        printf(1, "%-40s = %d\n", { "libvlc_media_get_meta_", libvlc_media_get_meta_})
        printf(1, "%-40s = %d\n", { "libvlc_media_set_meta_", libvlc_media_set_meta_})
        printf(1, "%-40s = %d\n", { "libvlc_media_save_meta_", libvlc_media_save_meta_})
        printf(1, "%-40s = %d\n", { "libvlc_media_parse_", libvlc_media_parse_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_new_", libvlc_media_player_new_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_player_new_from_media_", libvlc_media_player_new_from_media_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_player_release_", libvlc_media_player_release_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_media_", libvlc_media_player_set_media_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_media_", libvlc_media_player_get_media_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_player_is_playing_", libvlc_media_player_is_playing_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_player_play_", libvlc_media_player_play_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_pause_", libvlc_media_player_set_pause_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_pause_", libvlc_media_player_pause_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_stop_", libvlc_media_player_stop_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_xwindow_", libvlc_media_player_set_xwindow_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_xwindow_", libvlc_media_player_get_xwindow_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_hwnd_", libvlc_media_player_set_hwnd_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_hwnd_", libvlc_media_player_get_hwnd_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_length_", libvlc_media_player_get_length_})
--      printf(1, "%-40s = %d\n", { "libvlc_media_player_get_time_", libvlc_media_player_get_time_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_position_", libvlc_media_player_get_position_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_position_", libvlc_media_player_set_position_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_will_play_", libvlc_media_player_will_play_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_title_", libvlc_media_player_set_title_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_title_", libvlc_media_player_get_title_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_rate_", libvlc_media_player_get_rate_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_set_rate_", libvlc_media_player_set_rate_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_state_", libvlc_media_player_get_state_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_get_fps_", libvlc_media_player_get_fps_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_is_seekable_", libvlc_media_player_is_seekable_})
        printf(1, "%-40s = %d\n", { "libvlc_media_player_can_pause_", libvlc_media_player_can_pause_})
        printf(1, "%-40s = %d\n", { "libvlc_toggle_fullscreen_", libvlc_toggle_fullscreen_})
        printf(1, "%-40s = %d\n", { "libvlc_set_fullscreen_", libvlc_set_fullscreen_})
        printf(1, "%-40s = %d\n", { "libvlc_get_fullscreen_", libvlc_get_fullscreen_})
        printf(1, "%-40s = %d\n", { "libvlc_video_set_key_input_", libvlc_video_set_key_input_})
        printf(1, "%-40s = %d\n", { "libvlc_video_set_mouse_input_", libvlc_video_set_mouse_input_})
        printf(1, "%-40s = %d\n", { "libvlc_video_get_size_", libvlc_video_get_size_})
        printf(1, "%-40s = %d\n", { "libvlc_video_get_cursor_", libvlc_video_get_cursor_})
        printf(1, "%-40s = %d\n", { "libvlc_video_get_scale_", libvlc_video_get_scale_})
        printf(1, "%-40s = %d\n", { "libvlc_video_set_scale_", libvlc_video_set_scale_})
        printf(1, "%-40s = %d\n", { "libvlc_video_get_aspect_ratio_", libvlc_video_get_aspect_ratio_})
        printf(1, "%-40s = %d\n", { "libvlc_video_set_aspect_ratio_", libvlc_video_set_aspect_ratio_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_output_set_device_type_", libvlc_audio_output_set_device_type_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_toggle_mute_", libvlc_audio_toggle_mute_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_get_mute_", libvlc_audio_get_mute_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_set_mute_", libvlc_audio_set_mute_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_get_volume_", libvlc_audio_get_volume_})
        printf(1, "%-40s = %d\n", { "libvlc_audio_set_volume_", libvlc_audio_set_volume_})
end ifdef
--*/

--/*
--#include <stdio.h>
--#include <stdlib.h>
--#include <inttypes.h>
--#include <unistd.h>
--#include <vlc/vlc.h>
 
--int main(int argc, char* argv[])
--{
--  (void) argc; (void) argv;
--  libvlc_instance_t * inst;
--  libvlc_media_player_t *mp;
--  libvlc_media_t *m;
 
    /* Load the VLC engine */
    atom inst = libvlc_new (0, NULL);
 
    /* Create a new item */
--  m = libvlc_media_new_location("http://mycool.movie.com/test.mov");
    atom m = libvlc_media_new_path("/path/to/test.mov");
 
    /* Create a media player playing environement */
    atom mp = libvlc_media_player_new_from_media(inst, m);
 
    /* No need to keep the media now */
    libvlc_media_release(m);
 
    /* play the media_player */
    integer res = libvlc_media_player_play(mp)
    assert(res=0)
 
    while libvlc_media_player_is_playing(mp) do
        sleep(1)
        atom milliseconds = libvlc_media_player_get_time(mp);
--      int64_t seconds = milliseconds / 1000;
--      int64_t minutes = seconds / 60;
--      milliseconds -= seconds * 1000;
--      seconds -= minutes * 60;
-- 
--      printf("Current time: %" PRId64 ":%" PRId64 ":%" PRId64 "\n",
--             minutes, seconds, milliseconds);
        ?elapsed_short(milliseconds/1000)
    end while
 
    /* Stop playing */
--DEV 4.0+...
--  libvlc_media_player_stop_async(mp);
 
    /* Free the media_player */
    mp = libvlc_media_player_release(mp);
 
    libvlc_release (inst);
--      printf(1, "%-40s = %d\n", { "libvlc_media_get_duration_", libvlc_media_get_duration})
--LIBVLC_API libvlc_media_t *   libvlc_media_new_path (const char *path)
--LIBVLC_API void       libvlc_media_release (libvlc_media_t *p_md)
--LIBVLC_API libvlc_time_t      libvlc_media_get_duration (libvlc_media_t *p_md)
--Get duration (in ms) of media descriptor object item.
--
--Note, you need to call libvlc_media_parse_request() or play the media at least once before calling this function. Not doing this will result in an undefined result.
--
--Parameters
--p_md  media descriptor object
--Returns
--duration of media item or -1 on error
--typedef int64_t libvlc_time_t

-- libvlc_media_parse_local  = 0x01,
LIBVLC_API int libvlc_media_player_stop_async   (       libvlc_media_player_t *         p_mi    )       
Stop asynchronously.

Note
This function is asynchronous. In case of success, the user should wait for the libvlc_MediaPlayerStopped event to know when the stop is finished.
Parameters
p_mi    the Media Player
Returns
0 if the player is being stopped, -1 otherwise (no-op)
Version
LibVLC 4.0.0 or later
--*/

