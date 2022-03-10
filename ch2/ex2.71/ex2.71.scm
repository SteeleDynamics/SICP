;
; Exercise 2.71
; #############
; Suppose we have a Huffman tree for an alphabet of n symbols, and that the
; relative frequencies of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the tree
; for n=5; for n=10. In such a tree (for general n) how many bits are required
; to encode the most frequent symbol? the least frequent symbol?
;
; n=5:
; ----
; Alphabet: {A,B,C,D,E}
; Frequencies: ((A 16) (B 8) (C 4) (D 2) (E 1))
; Huffman Tree:
;
;      {A,B,C,D,E} 31
;           •
;          ╱ ╲ {B,C,D,E} 15
;        A 16 •
;            ╱ ╲ {C,D,E} 7
;          B 8  •
;              ╱ ╲ {D,E} 3
;            C 4  •
;                ╱ ╲
;              D 2 E 1
;
; n=10:
; -----
; Alphabet: {A,B,C,D,E,F,G,H,I,J}
; Frequencies: ((A 512) (B 256) (C 128) (D 64) (E 32)
;               (F 16) (G 8) (H 4) (I 2) (J 1))
; Huffman Tree:
;
;  {A,B,C,D,E,F,G,H,I,J} 1023
;           •
;          ╱ ╲ {B,C,D,E,F,G,H,I,J} 511
;       A 512 •
;            ╱ ╲ {C,D,E,F,G,H,I,J} 255
;         B 256 •
;              ╱ ╲ {D,E,F,G,H,I,J} 127
;           C 128 •
;                ╱ ╲ {E,F,G,H,I,J} 63
;              D 64 •
;                  ╱ ╲ {F,G,H,I,J} 31
;                E 32 •
;                    ╱ ╲ {G,H,I,J} 15
;                  F 16 •
;                      ╱ ╲ {H,I,J} 7
;                    G 8  •
;                        ╱ ╲ {I,J} 3
;                      H 4  •
;                          ╱ ╲
;                        I 2 J 1
;
; General Case:
; -------------
; Most Frequent Symbol ==> 1 bit, encoded as 0
; Least Frequent Symbol ==> n-1 bits, encoded as 111...1
;
