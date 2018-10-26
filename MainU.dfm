object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 197
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 17
    Top = 8
    Width = 104
    Height = 25
    Caption = 'View Only ON'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 17
    Top = 134
    Width = 185
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object RichEdit1: TRichEdit
    Left = 17
    Top = 39
    Width = 185
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    TabOrder = 2
    Zoom = 100
  end
  object CheckBox1: TCheckBox
    Left = 208
    Top = 70
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 3
  end
  object Button2: TButton
    Left = 208
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
  object ListBox1: TListBox
    Left = 304
    Top = 41
    Width = 145
    Height = 114
    ItemHeight = 13
    Items.Strings = (
      'Item 1'
      'Item 2'
      'Item 3'
      'Item 4')
    TabOrder = 5
  end
  object ScrollBar1: TScrollBar
    Left = 0
    Top = 180
    Width = 530
    Height = 17
    Align = alBottom
    PageSize = 0
    TabOrder = 6
    ExplicitLeft = -24
    ExplicitTop = 247
  end
end
