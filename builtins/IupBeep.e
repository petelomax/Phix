--
-- builtins/IupBeep.e
--
incomplete, basis for a new IupBeep(object f, d) function?
note I am thinking of IupBeep(261.63,0.5) and/or IupBeep({{261.63,293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25},0.5})
 [etc, in other words 1 or 2 args with one or more frequency/duration values, sq_op() and/or min()/max()-like]

from https://rosettacode.org/wiki/Musical_scale#Phix (version 1) [might struggle to make it an autoinclude...]

atom xBeep = 0
 
procedure beep(integer fi)
    if platform()=WINDOWS then
        integer frequency = floor(261.63 * power(2, fi/12)),
                duration = iff(fi == 12 ? 1000 : 500)
        if xBeep=0 then
            atom kernel32 = open_dll("kernel32.dll")
            xBeep = define_c_proc(kernel32, "Beep", {C_INT,C_INT})
        end if
        c_proc(xBeep,{frequency,duration})
    elsif platform()=LINUX then
        string play = sprintf("play -n -c1 synth 0.2 sin %%%d",fi-9)
        system(play)
    end if
end procedure
 
printf(1,"Please don't shoot the piano player, he's doing the best that he can!\n")
constant f = {0, 2, 4, 5, 7, 9, 11, 12}
for i=1 to length(f) do
    beep(f[i])
end for
printf(1,"That's all\n")

from pwa\js\poc\musical_scale.html -- modified copy of https://rosettacode.org/wiki/Musical_scale#JavaScript
                                    -- (obviously this ends up in pwa/builtins\IupBeep.js, as per <del>IupGraph.js</del> mpfr.js)
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Sample Page</title>
</head>
<body>
Upon loading the page you should hear the scale.
<button>Play</button>
<script type="text/javascript">
function musicalScale(freqArr){
    // create web audio api context
    var AudioContext = window.AudioContext || window.webkitAudioContext;
    var audioCtx = new AudioContext();
 
    // create oscillator and gain node
    var oscillator = audioCtx.createOscillator();
    var gainNode = audioCtx.createGain();
 
    // connect oscillator to gain node to speakers
    oscillator.connect(gainNode);
    gainNode.connect(audioCtx.destination);
 
    // set frequencies to play
    duration = 0.5   // seconds
    freqArr.forEach(function (freq, i){
        oscillator.frequency.setValueAtTime(freq, audioCtx.currentTime + i * duration);
    });
 
    // start playing!
    oscillator.start();
    // stop playing!
    oscillator.stop(audioCtx.currentTime + freqArr.length * duration);
}
 
function play() {
  musicalScale([261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25]);
}

const button = document.querySelector('button');
button.onclick = play;
</script>
</body>
</html>
