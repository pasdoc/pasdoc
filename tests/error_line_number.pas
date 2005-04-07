{ pasdoc should fail on parsing this unit, because the file "not_existing.inc"
  does not exist.
  The important thing is that failure message should indicate correct
  line number in this unit.

  There was a bug submitted to pasdoc-main list
  [http://sourceforge.net/mailarchive/forum.php?thread_id=6960993&forum_id=4647]
  that caused incorrect line number to be printed by pasdoc
  (lines between $ifdef NOT_DEFINED were not counted).
  Fixed with revision 1.18 of PasDoc_Tokenizer.pas
}

{$ifdef NOT_DEFINED}
A few lines here, please
1
2
3
4
5
6
7
8
9
10
{$endif}

{$I not_existing.inc}