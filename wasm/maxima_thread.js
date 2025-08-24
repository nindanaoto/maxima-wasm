var Module = {
    print: (function(text) {
        console.log(text);
    }),
    preRun: [],
    preInit: (function() {
	FS.createLazyFile('/', 'maxima-slatec.fas', 'maxima-slatec.fas', true, false);
	FS.createLazyFile('/', 'maxima-quadpack.fas', 'maxima-quadpack.fas', true, false);
	FS.createLazyFile('/', 'maxima-plot.fas', 'maxima-plot.fas', true, false);
	FS.createLazyFile('/', 'maxima-solve.fas', 'maxima-solve.fas', true, false);
	FS.createLazyFile('/', 'maxima-poisson-series.fas', 'maxima-poisson-series.fas', true, false);
	FS.createLazyFile('/', 'maxima-matrix-algebra.fas', 'maxima-matrix-algebra.fas', true, false);
	FS.createLazyFile('/', 'maxima-number-theory.fas', 'maxima-number-theory.fas', true, false);
	FS.createLazyFile('/', 'maxima-limits.fas', 'maxima-limits.fas', true, false);
	FS.createLazyFile('/', 'maxima-integration.fas', 'maxima-integration.fas', true, false);
	importScripts(location.origin + "/share_files.js");
    }),
    postRun: [],
};

onmessage = message => {
    let mutex = new Int32Array(message.data[0]);
    let inputBuffer = message.data[1];
    let inputBufferSizeBuffer = message.data[2];
    var index = 0;
    self.readChar = function () {
        let inputBufferSize = new Uint32Array(inputBufferSizeBuffer);
        while (index >= inputBufferSize[0]) {
            index = 0;
            mutex[0] = 0;
            self.postMessage({type: "waitingForInput"});
            Atomics.wait(mutex, 0, 0);
            inputBufferSize = new Uint32Array(inputBufferSizeBuffer);
        }
        let inputBufferArray = new Uint32Array(inputBuffer);
        return inputBufferArray[index++];
    }
    self.writeChar = function (c) {
        self.postMessage({type: "output", text: String.fromCodePoint(c)});
    }
    self.writeBaseString = function (char_pointer, size) {
        string = new Array(size);
        for (i = 0; i < size; i++) {
            string[i] = String.fromCodePoint(HEAP8[char_pointer + i]);
        }
        self.postMessage({type: "output", text: string.join('')});
    }
    self.convertString = function (char_pointer, size) {
        string = new Array(size);
        for (i = 0; i < size; i++) {
            string[i] = String.fromCodePoint(HEAP32[(char_pointer>>2) + i]);
        }
	return string.join('')
    }
    self.writeString = function (char_pointer, size) {
        self.postMessage({type: "output", text: convertString(char_pointer, size)});
    }
    self.displayOutput = function (label_plain, label_TeX, result_plain, result_TeX) {
	self.postMessage({type: "displayOutput", text: [label_plain, label_TeX, result_plain, result_TeX]});
    }
    self.displaySVG = function (filename) {
        var svg = FS.readFile(filename, {encoding: 'utf8'});
        self.postMessage({type: "displaySVG", text: svg});
    }
    self.checkForInterrupt = function () {
	index = 0;
        mutex[0] = 0;
        self.postMessage({type: "checkForInterrupt"});
        Atomics.wait(mutex, 0, 0);
        let inputBufferArray = new Uint32Array(inputBuffer);
        return inputBufferArray[0];
    }
    Module.print = function (s) {
	console.log(s);
    }
    importScripts(location.origin + "/maxima.js");
}

onerror = error => {
    console.error(error)
}
