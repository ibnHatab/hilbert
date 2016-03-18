Elm.Native.Teremin = {};
Elm.Native.Teremin.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Teremin = localRuntime.Native.Teremin || {};

    if (localRuntime.Native.Teremin.values)
    {
	return localRuntime.Native.Teremin.values;
    }

    var options = {
        waveform: 'square',
        filter: 'lowpass',
        delay: 0.300,
        feedback: 0.40
    };

    var context = null;
    var source = null;
    var nodes = {};

    function createWebAudioNodes() {
        nodes.oscVolume    = context.createGain();
        nodes.filter       = context.createBiquadFilter();
        nodes.volume       = context.createGain();
        nodes.delay        = context.createDelay();
        nodes.feedbackGain = context.createGain();
        nodes.compressor   = context.createDynamicsCompressor();
    }

    function routeSounds() {
        source = context.createOscillator();
        source.type = options.waveform;

        nodes.filter.type = options.filter;

        nodes.feedbackGain.gain.value = options.feedback;
        nodes.delay.delayTime.value = options.delay;
        nodes.volume.gain.value = 0.2;
        nodes.oscVolume.gain.value = 0;

        source.connect(nodes.oscVolume);
        nodes.oscVolume.connect(nodes.filter);
        nodes.filter.connect(nodes.compressor);
        nodes.filter.connect(nodes.delay);
        nodes.delay.connect(nodes.feedbackGain);
        nodes.delay.connect(nodes.compressor);
        nodes.feedbackGain.connect(nodes.delay);
        nodes.compressor.connect(nodes.volume);
        nodes.volume.connect(context.destination);
    }


    function setFrequency(val, range) {
        // min 40Hz
        var min = 4 * 40;
        // max half of the sampling rate 46.000.000
        var max = context.sampleRate / 100;
        // Logarithm (base 2) to compute how many octaves fall in the range.
        var numberOfOctaves = Math.log(max / min) / Math.LN2;
        // Compute a multiplier from 0 to 1 based on an exponential scale.
        var multiplier = Math.pow(2, numberOfOctaves * (((2 / range) * (range - val)) - 1.0));
        // Get back to the frequency value between min and max.
        var frequency = max * multiplier
        console.log(">> " + frequency)
        nodes.filter.frequency.value = frequency;
        source.frequency.value = frequency;
    }

    function setVolume(vol) {
        source.frequency.value = vol;
    }


    function init(args) {
        console.log("Initialize teremin." + args);

        try {
            window.AudioContext = window.AudioContext||window.webkitAudioContext;
            context = new AudioContext();
        }
        catch(e) {
            alert('Web Audio API is not supported in this browser');
        }

        createWebAudioNodes();
        routeSounds();

        source.start(0);
    }


    function startOsc(gain) {
        nodes.oscVolume.gain.value = gain;
    }

    function stopOsc(gain) {
        nodes.oscVolume.gain.value = gain;
    }

    function playSound(buffer, time) {
        var source = context.createBufferSource();
        source.buffer = buffer;
        source.connect(context.destination);
        source.start(time);
    }




    return localRuntime.Native.Teremin.values = {
        init: init,
        startOsc: startOsc,
        stopOsc: stopOsc,
        setFrequency: F2(setFrequency),
        setVolume: setVolume,
        playSound: playSound
    };
};
