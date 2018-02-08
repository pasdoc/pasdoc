{ Of course this unit is invalid, definition of InvalidConstant
  is suddenly truncated. So pasdoc should raise an error.

  But it's important on what line error is raisen.
  A previous bug caused the line to be "error_line_number_3.pas(13)",
  i.e. line number was not affected by dummy
  "function gtk_check_menu_item_get_active(" lines. }

unit error_line_number_3;

interface

{$ifdef NOT_DEFINED}
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
function gtk_check_menu_item_get_active(
{$endif}

const
  InvalidConstant:

end.