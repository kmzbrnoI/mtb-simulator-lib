object F_About: TF_About
  Left = 854
  Top = 589
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'About'
  ClientHeight = 168
  ClientWidth = 513
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object L_About1: TLabel
    Left = 0
    Top = 0
    Width = 513
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'Knihovnu vytvoril Michal Petrilak a Jan Horacek '#169' 2008 - 2013'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object L_About2: TLabel
    Left = 0
    Top = 32
    Width = 513
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Simulator Driver je urcen jako zobrazeni virtualnich vystupu'
  end
  object L_About3: TLabel
    Left = 0
    Top = 48
    Width = 513
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'desek MTB a jejich pinu nastavovanych programem pred knihovnou'
  end
  object ST_about4: TStaticText
    Left = 0
    Top = 72
    Width = 513
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Ve'#353'ker'#225' pr'#225'va k programu vyhrazena'
    TabOrder = 0
  end
  object ST_about5: TStaticText
    Left = 0
    Top = 96
    Width = 513
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Klub model'#225#345#367' '#382'eleznic Brno I '
    TabOrder = 1
  end
  object ST_about6: TStaticText
    Left = -8
    Top = 112
    Width = 521
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'http://www.kmz-brno.cz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 2
  end
  object ST_lib_Version: TStaticText
    Left = 0
    Top = 144
    Width = 513
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Verze Simulator knihovny : '
    TabOrder = 3
  end
end
