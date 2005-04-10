{ @abstract(First abstract)

  @abstract(Second abstract)

  Currently pasdoc ignores "First abstract" but doesn't warn about it.
  Should warn "You used @abstract twice in description of item ..."
  or something like that.

  Do it in TPasItem.StoreAbstractTag, save bool flag like AbstractSpecified
  (not check Description <> '', to allow "@abstract()").
  Right after pasdoc's release on sourceforge (2004-04-10). }

unit warning_abstract_twice;

interface

implementation

end.