
Server.default.boot;

//:

a = { WhiteNoise.ar(0.1) }.play;

//:

a.release(0.5);

//:

Server.default.quit;

//:

f = { | a | (a + 5).sqrt };

f.(10);