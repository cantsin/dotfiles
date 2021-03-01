final: prev: {
  st = prev.st.override {
    conf = builtins.readFile ./st/config.h;
    patches = [ ./st/badweight.patch ] ++ builtins.map final.fetchurl [
      {
        url =
          "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
        sha256 = "1cpap2jz80n90izhq5fdv2cvg29hj6bhhvjxk40zkskwmjn6k49j";
      }
      {
        url = "http://st.suckless.org/patches/anysize/st-anysize-0.8.1.diff";
        sha256 = "03z5vvajfbkpxvvk394799l94nbd8xk57ijq17hpmq1g1p2xn641";
      }
      {
        url =
          "https://st.suckless.org/patches/visualbell2/st-visualbell2-basic-2020-05-13-045a0fa.diff";
        sha256 = "05g46klchar4ahp6gz0mc5v4ydlsbi9wkjxdwyh4l5l4banax78n";
      }
    ];
  };
}
