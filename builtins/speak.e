--
--  builtins\speak.e
--  ================
--
--   See demo\rosetta\Speech.exw for example use, not documented beyond that.
--
requires(WINDOWS)
requires(32)    --  Windows 32 bit only, for now... 
-- (^ runs fine on a 64-bit OS, but needs a 32-bit p.exe)

include pComN.ew

procedure set_interest(atom pVoice, sequence flags)
    atom lo = #40000000,
         hi = #00000002
    for i=1 to length(flags) do
        lo += power(2,flags[i])
    end for
    atom res = cominvk(pVoice,ISpVoice_SetInterest,{lo,hi,lo,hi})
    if res!=S_OK then crash("invalid") end if
end procedure

atom pVoice = NULL

include cffi.e
constant tSPEVENT = """
typedef struct SPEVENT
{
    WORD         eEventId;
    WORD         elParamType;
    ULONG        ulStreamNum;
    ULONGLONG    ullAudioStreamOffset;
    WPARAM       wParam;
    LPARAM       lParam;
} SPEVENT;""",
idSPEVENT = define_struct(tSPEVENT),
pSPEVENT = allocate_struct(idSPEVENT)

function eventHandler(integer wParam, lParam)
    while cominvk(pVoice,ISpVoice_GetEvents,{1,pSPEVENT,NULL})=S_OK do
        integer eEventId = get_struct_field(idSPEVENT,pSPEVENT,"eEventId")
        if eEventId=SPEI_WORD_BOUNDARY then
--
-- SPEI_WORD_BOUNDARY: A word is beginning to synthesize. 
--  Markup language (XML) markers are counted in the boundaries and offsets. 
--  wParam is the character length of the word in the current input stream being synthesized. 
--  lParam is the character position within the current text input stream of the word being synthesized.
-- Note: eventHandler(wParam is length(rate_prefix) as passed to ISpVoice_SetNotifyCallbackFunction,
--                   ,lParam is a (non-0)routine_id                 "", 
--       whereas event.w/lParam fields are len/pos, where event is from ISpVoice_GetEvents.
--
            integer len = get_struct_field(idSPEVENT,pSPEVENT,"wParam"),
                    pos = get_struct_field(idSPEVENT,pSPEVENT,"lParam")
            pos -= wParam -- (==length(rate_prefix))
            lParam(pos,len)
        elsif eEventId=SPEI_END_INPUT_STREAM then
            lParam(-1,0)    
        end if
    end while
    return 0
end function
constant cb_eventHandler = call_back(eventHandler)

global procedure speak(string text, atom rate=0, integer speech_cb=0)
    atom res
    if pVoice=NULL then
--      --<no idea why we need this>
--      --if there is an include pGUI.e and an IupOpen():
--      --  OK with neither, OK with both, 2nd only BOOM
--      --else
--      --  BOOM with neither, OK with both, 2nd only OK.
--      --end if
--      --Then again, user activation is required in a 
--      --browser, which means we need pGUI.e anyway.
--      CoUnInitialize()
--      CoInitializeEx()
--      -- </no idea>
        pVoice = allocate(machine_word())
        res = CoCreateInstance(CLSID_SpVoice,IID_ISpVoice,pVoice)
        if res!=S_OK then
            crash("Failed to initialize SpeechAPI. (%08x)\n",res)
        end if
    end if
    string rate_prefix = iff(rate=0?"":sprintf(`<rate speed="%d">`,rate))
    if speech_cb!=0 then
        res = cominvk(pVoice,ISpVoice_SetNotifyCallbackFunction,{cb_eventHandler,length(rate_prefix),speech_cb})
        assert(res=S_OK,"Error setting callback...")
        set_interest(pVoice, {SPEI_WORD_BOUNDARY,SPEI_END_INPUT_STREAM})
    end if
    res = cominvk(pVoice,ISpVoice_Speak,{unicode_string(rate_prefix&text),SPF_IS_XML+SPF_ASYNC,0})
end procedure

--/*
typedef enum SPEVENTENUM
{
    SPEI_UNDEFINED,

    //--- TTS engine
    SPEI_START_INPUT_STREAM,
    SPEI_END_INPUT_STREAM,
    SPEI_VOICE_CHANGE,
    SPEI_TTS_BOOKMARK,
    SPEI_WORD_BOUNDARY,
    SPEI_PHONEME,
    SPEI_SENTENCE_BOUNDARY,
    SPEI_VISEME,
    SPEI_TTS_AUDIO_LEVEL,

    //--- Engine vendors use these reserved bits
    SPEI_TTS_PRIVATE,
    SPEI_MIN_TTS,
    SPEI_MAX_TTS,

    //--- Speech Recognition
    SPEI_END_SR_STREAM,
    SPEI_SOUND_START,
    SPEI_SOUND_END,
    SPEI_PHRASE_START,
    SPEI_RECOGNITION,
    SPEI_HYPOTHESIS,
    SPEI_SR_BOOKMARK,
    SPEI_PROPERTY_NUM_CHANGE,
    SPEI_PROPERTY_STRING_CHANGE,
    SPEI_FALSE_RECOGNITION,
    SPEI_INTERFERENCE,
    SPEI_REQUEST_UI,
    SPEI_RECO_STATE_CHANGE,
    SPEI_ADAPTATION,
    SPEI_START_SR_STREAM,
    SPEI_RECO_OTHER_CONTEXT,
    SPEI_SR_AUDIO_LEVEL,
    SPEI_SR_RETAINEDAUDIO,

    //--- Engine vendors use these reserved bits
    SPEI_SR_PRIVATE,
    SPEI_MIN_SR,
    SPEI_MAX_SR,

    //--- Reserved: Do not use
    SPEI_RESERVED1,
    SPEI_RESERVED2,
    SPEI_RESERVED3
} SPEVENTENUM;

typedef struct SPEVENT
{
    WORD         eEventId;      -- see SPEVENTENUM
    WORD         elParamType;
    ULONG        ulStreamNum;
    ULONGLONG    ullAudioStreamOffset;
    WPARAM       wParam;
    LPARAM       lParam;
} SPEVENT;

HRESULT GetEvents(
   ULONG     ulCount,
   SPEVENT  *pEventArray,
   ULONG    *pulFetched
);
--*/
