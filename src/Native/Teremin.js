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
        delay: 0.500,
        feedback: 0.4
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
        playSound: playSound
    };
};
