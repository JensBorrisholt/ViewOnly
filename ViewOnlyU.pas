unit ViewOnlyU;

interface

(*
  Author : Jens Borrisholt
  E Mail : Jens@Borrisholt.com
  Date   : December 3rd 2008

  For the author's protection, I want to make certain that everyone
  understands that there is no warranty for this free software.

  If the software is modified by someone else and passed on, I want it's
  recipients to know that what they have is not the original, so that any problems
  introduced by others will not reflect on the original authors' reputations.

  You may use this component in all kinds of software, but please do not
  remove my name from the component.

  ************************************************************************************
  *****************************  Example of use **************************************
  ************************************************************************************

  Take a form, new or existing
  if a new form then add some visible controls to it.

  Add ViewOnlyU to the uses clause

  Add a button on your form, lets call the button BtnViewOnly

  On the FromCreate Event type
  BtnViewOnly.CanViewOnly := False;

  Add a OnClick handler to BtnViewOnly, and Implement it :

  procedure TMainform.BtnViewOnlyClick(Sender: TObject);
  var
    i : Integer;
  begin
    for i := 0 to ComponentCount -1 do
      if Components[i] is TWinControl then
        TWinControl(Components[i]).ViewOnly := not TWinControl(Components[i]).ViewOnly;
  end;

  You now have a form with a ViewOnlyMode, automatic applied to is and all it's sub controls.
*)
uses
  Controls, Classes;

type
  TWinControlHelper = class Helper for TWinControl
  strict private
    function GetHookControl: TObject;
    function GetViewOnly: Boolean;
    procedure SetViewOnly(const Value: Boolean);
    function GetCanViewOnly: Boolean;
    procedure SetCanViewOnly(const Value: Boolean);
  public
    property ViewOnly: Boolean read GetViewOnly write SetViewOnly;
    property CanViewOnly: Boolean read GetCanViewOnly write SetCanViewOnly;
  end;

implementation

uses
  Windows, Messages, Forms;

{$M+}

type
  THackWinControl = class(TWinControl);

  THookControl = class(TComponent)
  strict private
    FWinControl: TWinControl;
    FViewOnly: Boolean;
    FOriginalWindowProc: TWndMethod;
    FCanViewOnly: Boolean;
    procedure WMSetCursor(var Message: TWMSetCursor);
    procedure SetWinControl(const Value: TWinControl);
    procedure SetViewOnly(const Value: Boolean);
    procedure SetCanViewOnly(const Value: Boolean);
  strict protected
    procedure NewWndProc(var Message: TMessage); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TWinControl); reintroduce;
  published
    property WinControl: TWinControl read FWinControl write SetWinControl;
    property ViewOnly: Boolean read FViewOnly write SetViewOnly default False;
    property CanViewOnly: Boolean read FCanViewOnly write SetCanViewOnly;
  end;

  { TWinControlHelper }

function TWinControlHelper.GetCanViewOnly: Boolean;
var
  HookControl: THookControl;
begin
  HookControl := THookControl(GetHookControl);
  Result := (HookControl <> nil) and (HookControl.CanViewOnly);
end;

function TWinControlHelper.GetHookControl: TObject;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is THookControl) then
      if THookControl(Components[I]).WinControl = Self then
        Exit(Components[I]);

  Result := nil;
end;

function TWinControlHelper.GetViewOnly: Boolean;
var
  HookControl: THookControl;
begin
  HookControl := THookControl(GetHookControl);
  Result := (HookControl <> nil) and (HookControl.ViewOnly);
end;

procedure TWinControlHelper.SetCanViewOnly(const Value: Boolean);
var
  HookControl: THookControl;
begin
  HookControl := THookControl(GetHookControl);
  if HookControl = nil then
  begin
    SetViewOnly(False);
    HookControl := THookControl(GetHookControl);
  end;

  HookControl.CanViewOnly := Value;
end;

procedure TWinControlHelper.SetViewOnly(const Value: Boolean);
var
  I: Integer;
  HookControl: THookControl;
begin
  HookControl := THookControl(GetHookControl);

  if HookControl = nil then
  begin
    HookControl := THookControl.Create(Self);
    InsertComponent(HookControl);
  end;

  HookControl.ViewOnly := Value;
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TWinControl) then
      TWinControl(Components[I]).ViewOnly := Value;
end;

{ THookControl }

constructor THookControl.Create(AOwner: TWinControl);
begin
  WinControl := AOwner;
  FCanViewOnly := True;
  inherited Create(AOwner);
end;

procedure THookControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if AComponent = FWinControl then
    if Operation = opRemove then
      WinControl := nil; // UnHook WinControl and release you self
end;

procedure THookControl.SetWinControl(const Value: TWinControl);
begin
  if FWinControl = Value then
    Exit;

  if Value <> nil then
  begin
    FWinControl := Value;
    FWinControl.FreeNotification(Self);
    FOriginalWindowProc := FWinControl.WindowProc;
    FWinControl.WindowProc := NewWndProc;
  end
  else
  begin
    FWinControl.WindowProc := FOriginalWindowProc;
    FWinControl.RemoveFreeNotification(Self);
    Destroy;
  end;
end;

procedure THookControl.SetCanViewOnly(const Value: Boolean);
begin
  FCanViewOnly := Value;
end;

procedure THookControl.SetViewOnly(const Value: Boolean);
begin
  if not FCanViewOnly then
    Exit;

  if FViewOnly = Value then
    Exit;

  FViewOnly := Value;

  if not Value then
    ShowCaret(FWinControl.Handle);

  FWinControl.Invalidate;
end;

procedure THookControl.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  if not ViewOnly then
    Exit;

  Message.Result := 1;
  GetCursorPos(P);

  with PointToSmallPoint(P) do
    case FWinControl.Perform(WM_NCHITTEST, 0, MakeLong(X, Y)) of
      HTVSCROLL, HTHSCROLL:
        Windows.SetCursor(Screen.Cursors[crArrow]);
      HTCLIENT:
        Windows.SetCursor(Screen.Cursors[FWinControl.Cursor]);
    end;
end;

procedure THookControl.NewWndProc(var Message: TMessage);

  procedure Scroll(MSG, ScrollCode: Integer);
  begin
    FWinControl.Perform(MSG, ScrollCode, 0);
    FWinControl.Perform(MSG, SB_ENDSCROLL, 0);
  end;

begin
  if (not ViewOnly) or (InheritsFrom(TForm)) then
  begin
    FOriginalWindowProc(Message);
    Exit;
  end;

  case Message.MSG of
    WM_PAINT:
      HideCaret(FWinControl.Handle);

    CM_DIALOGCHAR:
      begin
        Message.Result := 1;
        Exit;
      end;

    WM_SETCURSOR:
      begin
        WMSetCursor(TWMSetCursor(Message));
        Exit;
      end;

    WM_SETFOCUS:
      begin
        HideCaret(FWinControl.Handle);
        Exit;
      end;

    WM_KILLFOCUS:
      begin
        ShowCaret(FWinControl.Handle);
        Exit;
      end;

    WM_MOUSEFIRST .. WM_MOUSELAST, WM_CHAR, WM_KEYUP:
      begin
        Message.Result := 0;
        if Message.MSG = WM_LBUTTONDOWN then
          if not FWinControl.Focused then
            FWinControl.SetFocus;
        Exit;
      end;

    WM_KEYDOWN:
      begin
        case Message.WPARAM of
          VK_DOWN:
            Scroll(WM_VSCROLL, SB_LINEDOWN);
          VK_UP:
            Scroll(WM_VSCROLL, SB_LINEUP);
          VK_LEFT:
            Scroll(WM_HSCROLL, SB_LINELEFT);
          VK_RIGHT:
            Scroll(WM_HSCROLL, SB_LINERIGHT);
          VK_NEXT:
            Scroll(WM_VSCROLL, SB_PAGEDOWN);
          VK_PRIOR:
            Scroll(WM_VSCROLL, SB_PAGEUP);
          VK_HOME:
            Scroll(WM_VSCROLL, SB_TOP);
          VK_END:
            Scroll(WM_VSCROLL, SB_BOTTOM);
        end;

        Message.Result := 0;
        THackWinControl(FWinControl).DoKeyDown(TWMkey(Message)); // call the original event handler
        Exit;
      end;
  end; // case

  FOriginalWindowProc(Message);
end;

end.
