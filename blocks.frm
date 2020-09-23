VERSION 5.00
Begin VB.Form Block 
   BackColor       =   &H80000006&
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   4875
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7455
   Icon            =   "blocks.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4875
   ScaleWidth      =   7455
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox smokescreen 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   3855
      Left            =   2880
      ScaleHeight     =   3855
      ScaleWidth      =   3855
      TabIndex        =   145
      Top             =   480
      Width           =   3855
      Begin VB.Label ready2 
         BackColor       =   &H00000000&
         Caption         =   "10"
         BeginProperty Font 
            Name            =   "BankGothic Md BT"
            Size            =   48
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00800000&
         Height          =   1095
         Left            =   720
         TabIndex        =   147
         Top             =   1560
         Width           =   1575
      End
      Begin VB.Label ready 
         BackColor       =   &H80000012&
         Caption         =   "GET READY FOR LEVEL:"
         BeginProperty Font 
            Name            =   "BankGothic Md BT"
            Size            =   20.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00800000&
         Height          =   855
         Left            =   240
         TabIndex        =   146
         Top             =   480
         Width           =   2895
      End
   End
   Begin VB.PictureBox liveblock 
      Appearance      =   0  'Flat
      BackColor       =   &H00800000&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   360
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   143
      Top             =   3360
      Width           =   495
   End
   Begin VB.PictureBox stmenu 
      Appearance      =   0  'Flat
      BackColor       =   &H80000006&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   0
      Picture         =   "blocks.frx":0442
      ScaleHeight     =   735
      ScaleWidth      =   735
      TabIndex        =   140
      Top             =   0
      Width           =   735
      Begin VB.Label Label8 
         BackColor       =   &H80000012&
         Caption         =   "By : Rick Hagen"
         BeginProperty Font 
            Name            =   "BankGothic Md BT"
            Size            =   12
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000080&
         Height          =   255
         Left            =   120
         TabIndex        =   149
         Top             =   4200
         Width           =   2415
      End
      Begin VB.Label Label7 
         BackColor       =   &H80000012&
         Caption         =   "Exit"
         BeginProperty Font 
            Name            =   "BankGothic Md BT"
            Size            =   24
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000080&
         Height          =   495
         Left            =   3240
         TabIndex        =   142
         Top             =   3120
         Width           =   1215
      End
      Begin VB.Label Label6 
         BackColor       =   &H80000007&
         Caption         =   "Start"
         BeginProperty Font 
            Name            =   "BankGothic Md BT"
            Size            =   24
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000080&
         Height          =   495
         Left            =   3000
         TabIndex        =   141
         Top             =   2160
         Width           =   1695
      End
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   4680
      Top             =   0
   End
   Begin VB.PictureBox car 
      Appearance      =   0  'Flat
      BackColor       =   &H80000013&
      ForeColor       =   &H80000008&
      Height          =   500
      Left            =   6240
      ScaleHeight     =   465
      ScaleWidth      =   465
      TabIndex        =   131
      Top             =   480
      Width           =   500
   End
   Begin VB.PictureBox opp 
      Appearance      =   0  'Flat
      BackColor       =   &H00000080&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   2880
      ScaleHeight     =   465
      ScaleWidth      =   465
      TabIndex        =   130
      Top             =   3840
      Width           =   495
   End
   Begin VB.PictureBox p131 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   129
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p130 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   128
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p126 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   126
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
      Begin VB.PictureBox Picture5 
         Height          =   15
         Left            =   480
         ScaleHeight     =   15
         ScaleWidth      =   255
         TabIndex        =   127
         Top             =   240
         Width           =   255
      End
   End
   Begin VB.PictureBox p127 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   125
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p128 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   124
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p129 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   123
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p124 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   122
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p125 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   121
      Top             =   2400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p116 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   120
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p117 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   119
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p118 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   118
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p119 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   117
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p120 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   116
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p121 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   115
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p122 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   114
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p123 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   113
      Top             =   2640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p108 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   112
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p109 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   111
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p110 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   110
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p111 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   109
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p112 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   108
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p113 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   107
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p114 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   106
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p115 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   105
      Top             =   2880
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p100 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   104
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p101 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   103
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p102 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   102
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p103 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   101
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p104 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   100
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p105 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   99
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p106 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   98
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p107 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   97
      Top             =   2160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p15 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   96
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p5 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   95
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p9 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   94
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p11 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   93
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p13 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   92
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p7 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   91
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p3 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   90
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   89
      Top             =   480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p50 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   88
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p51 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   87
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p52 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   86
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p53 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   85
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p54 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   84
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p55 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   83
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p56 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   82
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p57 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   81
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p58 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   80
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p59 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   79
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p60 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   78
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p61 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   77
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p63 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   76
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p64 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   75
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p65 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   74
      Top             =   3120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p66 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   73
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p67 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   72
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p68 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   71
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p69 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   70
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p70 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   69
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p71 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   68
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p72 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   67
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p73 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   66
      Top             =   3360
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p74 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   65
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p75 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   64
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p76 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   63
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p78 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   62
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p79 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   61
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p80 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   60
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p81 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   59
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p82 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   58
      Top             =   3600
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p83 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   57
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p92 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   56
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p84 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   55
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p85 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   54
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p86 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   53
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p87 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   52
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p89 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   51
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p90 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   50
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p91 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   49
      Top             =   3840
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p93 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   48
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p94 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   47
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p95 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   46
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p96 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   45
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p97 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   44
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p98 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   43
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p99 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   42
      Top             =   4080
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p17 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   41
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p18 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   40
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p19 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   39
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p20 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   38
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p21 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   37
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p22 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   36
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p23 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   35
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p24 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   34
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p25 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   33
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p26 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   32
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p27 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   31
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p28 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   30
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p29 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   29
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p30 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   28
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p31 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   27
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p32 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   26
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p33 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   25
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p34 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3360
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   24
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p35 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3840
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   23
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p36 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4320
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   22
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p37 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   21
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p38 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   20
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p39 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5760
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   19
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p40 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6240
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   18
      Top             =   1440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p41 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   17
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p42 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   16
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p43 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   15
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p44 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   14
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p45 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   13
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p46 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   12
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p47 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   11
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p48 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   10
      Top             =   1680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p49 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   9
      Top             =   1920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p2 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   8
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p4 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3600
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   7
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p6 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4080
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   6
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p8 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   5
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p10 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5040
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   4
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p12 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5520
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   3
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p14 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6000
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   2
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox p16 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   6480
      ScaleHeight     =   225
      ScaleWidth      =   225
      TabIndex        =   1
      Top             =   720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.PictureBox cage 
      Appearance      =   0  'Flat
      BackColor       =   &H00400040&
      BorderStyle     =   0  'None
      DrawStyle       =   4  'Dash-Dot-Dot
      ForeColor       =   &H80000001&
      Height          =   3860
      Left            =   2880
      ScaleHeight     =   3855
      ScaleWidth      =   3855
      TabIndex        =   0
      Top             =   480
      Width           =   3860
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   10
      Left            =   2760
      Top             =   -120
   End
   Begin VB.Timer tmrKey 
      Left            =   3480
      Top             =   -120
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000012&
      Caption         =   "0"
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   240
      TabIndex        =   148
      Top             =   2400
      Visible         =   0   'False
      Width           =   15
   End
   Begin VB.Label lives 
      BackColor       =   &H80000007&
      Caption         =   "7"
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   36
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000013&
      Height          =   615
      Left            =   1560
      TabIndex        =   144
      Top             =   3240
      Width           =   735
   End
   Begin VB.Line Line2 
      BorderColor     =   &H000000C0&
      BorderWidth     =   3
      X1              =   1320
      X2              =   1080
      Y1              =   3600
      Y2              =   3840
   End
   Begin VB.Line Line1 
      BorderColor     =   &H000000C0&
      BorderWidth     =   3
      X1              =   1080
      X2              =   1320
      Y1              =   3600
      Y2              =   3840
   End
   Begin VB.Label Label5 
      BackColor       =   &H80000008&
      Caption         =   "Label5"
      Height          =   615
      Left            =   6840
      TabIndex        =   139
      Top             =   3720
      Width           =   1695
   End
   Begin VB.Label spedo 
      BackColor       =   &H80000007&
      Caption         =   "0"
      Height          =   15
      Left            =   0
      TabIndex        =   138
      Top             =   6240
      Width           =   1095
   End
   Begin VB.Label counter 
      BackColor       =   &H80000007&
      Caption         =   "5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   735
      Left            =   5760
      TabIndex        =   137
      Top             =   120
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.Label block 
      BackColor       =   &H80000012&
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   255
      Left            =   5160
      TabIndex        =   136
      Top             =   4440
      Width           =   1095
   End
   Begin VB.Label Label4 
      BackColor       =   &H80000012&
      Caption         =   "Blocks Left :"
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   255
      Left            =   2880
      TabIndex        =   135
      Top             =   4440
      Width           =   2175
   End
   Begin VB.Label level 
      BackColor       =   &H80000012&
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   375
      Left            =   4080
      TabIndex        =   134
      Top             =   120
      Width           =   1215
   End
   Begin VB.Label Label3 
      BackColor       =   &H80000012&
      Caption         =   "Level:"
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   375
      Left            =   2880
      TabIndex        =   133
      Top             =   120
      Width           =   1095
   End
   Begin VB.Label Label2 
      BackColor       =   &H80000012&
      Caption         =   "Main Menu"
      BeginProperty Font 
         Name            =   "BankGothic Md BT"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000080&
      Height          =   255
      Left            =   240
      TabIndex        =   132
      Top             =   1560
      Width           =   1695
   End
End
Attribute VB_Name = "Block"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

 Dim dx As New DirectX7
 Dim di As DirectInput
 Dim diDEV As DirectInputDevice
 Dim diState As DIKEYBOARDSTATE
 Dim iKeyCounter As Integer

Private Sub Form_Load()
Set di = dx.DirectInputCreate()
     If Err.Number <> 0 Then
         MsgBox "Error starting Direct Input, please make sure you have DirectX installed", vbApplicationModal
         End
     End If
     Set diDEV = di.CreateDevice("GUID_SysKeyboard")
     diDEV.SetCommonDataFormat DIFORMAT_KEYBOARD
     diDEV.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE 'Lets it share with
     Me.Show
     diDEV.Acquire
     tmrKey.Interval = 10
     tmrKey.Enabled = True
     stmenu.Height = 5895
     stmenu.Width = 7455
     liveblock.Visible = False
     smokescreen.Visible = False
     
          End Sub
Private Sub Form_Unload(Cancel As Integer)
 diDEV.Unacquire
End Sub

Private Sub Label2_Click()
level.Caption = ""
ready.Visible = False
ready2.Visible = False
newgame
End Sub

Private Sub Label5_Click()
nextlevel
End Sub

Private Sub Label6_Click()
stmenu.Width = 20
stmenu.Left = 0
stmenu.Top = 0
stmenu.Height = 20
lives.Caption = "7"
liveblock.Visible = True
smokescreen.Visible = True
nextlevel
End Sub

Private Sub Label7_Click()
End
End Sub

Private Sub Timer1_Timer()
Timer1.Enabled = True
If level.Caption = "ONE" Then
Let speedo = 10
End If
If level.Caption = "TWO" Then
Let speedo = 10
End If
If level.Caption = "THREE" Then
Let speedo = 10
End If
If level.Caption = "FOUR" Then
Let speedo = 20
End If
If level.Caption = "FIVE" Then
Let speedo = 20
End If
If level.Caption = "SIX" Then
Let speedo = 30
End If
If level.Caption = "SEVEN" Then
Let speedo = 40
End If
If level.Caption = "EIGHT" Then
Let speedo = 40
End If
If level.Caption = "NINE" Then
Let speedo = 60
End If
If level.Caption = "TEN" Then
Let speedo = spedo.Caption
End If
If spedo.Caption < 80 Then
spedo.Caption = (spedo.Caption + 1)
End If
If (opp.Left + opp.Width) / 2 > (car.Left + car.Width) / 2 Then
opp.Left = opp.Left - speedo
End If
If (opp.Left + opp.Width) / 2 < (car.Left + car.Width) / 2 Then
opp.Left = opp.Left + speedo
End If
If (opp.Top + opp.Height) / 2 > (car.Top + car.Height) / 2 Then
opp.Top = opp.Top - speedo
End If
If (opp.Top + opp.Height) / 2 < (car.Top + car.Height) / 2 Then
opp.Top = opp.Top + speedo
End If
If car.Left <= opp.Left + opp.Width And car.Left >= opp.Left And car.Top <= opp.Top + opp.Height And car.Top >= opp.Top Then
resert
End If
If car.Left + car.Width >= opp.Left And car.Top <= opp.Top + opp.Height And car.Left + car.Width <= opp.Left + opp.Width And car.Top >= opp.Top Then
resert
End If
If car.Top + car.Height >= opp.Top And car.Top + car.Height <= opp.Top + opp.Height And car.Left + car.Width >= opp.Left And car.Left + car.Width <= opp.Left + opp.Width Then
resert
End If
If car.Top + car.Height >= opp.Top And car.Top + car.Height <= opp.Top + opp.Height And car.Left >= opp.Left And car.Left <= opp.Left + opp.Width Then
resert
End If
End Sub



Private Sub Timer2_Timer()
counter.Caption = counter.Caption - 1
Timer1.Enabled = False
If counter.Caption = "0" Then
Timer2.Enabled = False
ready.Visible = False
ready2.Visible = False
opp.Top = cage.Top
opp.Left = p15.Left
car.Top = p83.Top
car.Left = cage.Left
Timer1.Enabled = True
tmrKey.Enabled = True
smokescreen.Visible = False
End If
End Sub

Private Sub tmrKey_Timer()
    diDEV.GetDeviceStateKeyboard diState

     For iKeyCounter = 0 To 255
         If diState.Key(iKeyCounter) <> 0 Then
             

Let cup = 120
Let cdown = 120
Let cleft = 120
Let cright = 120
If car.Top = cage.Top Then
cup = 0
End If
If car.Top + car.Height = cage.Top + cage.Height Then
cdown = 0
End If
If car.Top < cage.Top And car.Top + car.Height > cage.Top + cage.Height Then
cup = 120
cdown = 120
End If
If car.Left = cage.Left Then
cleft = 0
End If
If car.Left + car.Width = cage.Width + cage.Left Then
cright = 0
End If
If car.Left > cage.Left And car.Left + car.Width < cage.Left + cage.Width Then
cright = 120
cleft = 120
End If
If car.Left <= p1.Left And car.Left + car.Width >= p1.Width + p1.Left And car.Top <= p1.Top And car.Top + car.Height >= p1.Top + p1.Height Then
If p1.Visible = True Then
block.Caption = block.Caption - 1
End If
p1.Visible = False
End If
If car.Left <= p2.Left And car.Left + car.Width >= p2.Width + p2.Left And car.Top <= p2.Top And car.Top + car.Height >= p2.Top + p2.Height Then
If p2.Visible = True Then
block.Caption = block.Caption - 1
End If
p2.Visible = False
End If
If car.Left <= p3.Left And car.Left + car.Width >= p3.Width + p3.Left And car.Top <= p3.Top And car.Top + car.Height >= p3.Top + p3.Height Then
If p3.Visible = True Then
block.Caption = block.Caption - 1
End If
p3.Visible = False
End If
If car.Left <= p4.Left And car.Left + car.Width >= p4.Width + p4.Left And car.Top <= p4.Top And car.Top + car.Height >= p4.Top + p4.Height Then
If p4.Visible = True Then
block.Caption = block.Caption - 1
End If
p4.Visible = False
End If
If car.Left <= p5.Left And car.Left + car.Width >= p5.Width + p5.Left And car.Top <= p5.Top And car.Top + car.Height >= p5.Top + p5.Height Then
If p5.Visible = True Then
block.Caption = block.Caption - 1
End If
p5.Visible = False
End If
If car.Left <= p6.Left And car.Left + car.Width >= p6.Width + p6.Left And car.Top <= p6.Top And car.Top + car.Height >= p6.Top + p6.Height Then
If p6.Visible = True Then
block.Caption = block.Caption - 1
End If
p6.Visible = False
End If
If car.Left <= p7.Left And car.Left + car.Width >= p7.Width + p7.Left And car.Top <= p7.Top And car.Top + car.Height >= p7.Top + p7.Height Then
If p7.Visible = True Then
block.Caption = block.Caption - 1
End If
p7.Visible = False
End If
If car.Left <= p8.Left And car.Left + car.Width >= p8.Width + p8.Left And car.Top <= p8.Top And car.Top + car.Height >= p8.Top + p8.Height Then
If p8.Visible = True Then
block.Caption = block.Caption - 1
End If
p8.Visible = False
End If
If car.Left <= p9.Left And car.Left + car.Width >= p9.Width + p9.Left And car.Top <= p9.Top And car.Top + car.Height >= p9.Top + p9.Height Then
If p9.Visible = True Then
block.Caption = block.Caption - 1
End If
p9.Visible = False
End If
If car.Left <= p10.Left And car.Left + car.Width >= p10.Width + p10.Left And car.Top <= p10.Top And car.Top + car.Height >= p10.Top + p10.Height Then
If p10.Visible = True Then
block.Caption = block.Caption - 1
End If
p10.Visible = False
End If
If car.Left <= p11.Left And car.Left + car.Width >= p11.Width + p11.Left And car.Top <= p11.Top And car.Top + car.Height >= p11.Top + p11.Height Then
If p11.Visible = True Then
block.Caption = block.Caption - 1
End If
p11.Visible = False
End If
If car.Left <= p12.Left And car.Left + car.Width >= p12.Width + p12.Left And car.Top <= p12.Top And car.Top + car.Height >= p12.Top + p12.Height Then
If p12.Visible = True Then
block.Caption = block.Caption - 1
End If
p12.Visible = False
End If
If car.Left <= p13.Left And car.Left + car.Width >= p13.Width + p13.Left And car.Top <= p13.Top And car.Top + car.Height >= p13.Top + p13.Height Then
If p13.Visible = True Then
block.Caption = block.Caption - 1
End If
p13.Visible = False
End If
If car.Left <= p14.Left And car.Left + car.Width >= p14.Width + p14.Left And car.Top <= p14.Top And car.Top + car.Height >= p14.Top + p14.Height Then
If p14.Visible = True Then
block.Caption = block.Caption - 1
End If
p14.Visible = False
End If
If car.Left <= p15.Left And car.Left + car.Width >= p15.Width + p15.Left And car.Top <= p15.Top And car.Top + car.Height >= p15.Top + p15.Height Then
If p15.Visible = True Then
block.Caption = block.Caption - 1
End If
p15.Visible = False
End If
If car.Left <= p16.Left And car.Left + car.Width >= p16.Width + p16.Left And car.Top <= p16.Top And car.Top + car.Height >= p16.Top + p16.Height Then
If p16.Visible = True Then
block.Caption = block.Caption - 1
End If
p16.Visible = False
End If
If car.Left <= p17.Left And car.Left + car.Width >= p17.Width + p17.Left And car.Top <= p17.Top And car.Top + car.Height >= p17.Top + p17.Height Then
If p17.Visible = True Then
block.Caption = block.Caption - 1
End If
p17.Visible = False
End If
If car.Left <= p18.Left And car.Left + car.Width >= p18.Width + p18.Left And car.Top <= p18.Top And car.Top + car.Height >= p18.Top + p18.Height Then
If p18.Visible = True Then
block.Caption = block.Caption - 1
End If
p18.Visible = False
End If
If car.Left <= p19.Left And car.Left + car.Width >= p19.Width + p19.Left And car.Top <= p19.Top And car.Top + car.Height >= p19.Top + p19.Height Then
If p19.Visible = True Then
block.Caption = block.Caption - 1
End If
p19.Visible = False
End If
If car.Left <= p20.Left And car.Left + car.Width >= p20.Width + p20.Left And car.Top <= p20.Top And car.Top + car.Height >= p20.Top + p20.Height Then
If p20.Visible = True Then
block.Caption = block.Caption - 1
End If
p20.Visible = False
End If
If car.Left <= p21.Left And car.Left + car.Width >= p21.Width + p21.Left And car.Top <= p21.Top And car.Top + car.Height >= p21.Top + p21.Height Then
If p21.Visible = True Then
block.Caption = block.Caption - 1
End If
p21.Visible = False
End If
If car.Left <= p22.Left And car.Left + car.Width >= p22.Width + p22.Left And car.Top <= p22.Top And car.Top + car.Height >= p22.Top + p22.Height Then
If p22.Visible = True Then
block.Caption = block.Caption - 1
End If
p22.Visible = False
End If
If car.Left <= p23.Left And car.Left + car.Width >= p23.Width + p23.Left And car.Top <= p23.Top And car.Top + car.Height >= p23.Top + p23.Height Then
If p23.Visible = True Then
block.Caption = block.Caption - 1
End If
p23.Visible = False
End If
If car.Left <= p24.Left And car.Left + car.Width >= p24.Width + p24.Left And car.Top <= p24.Top And car.Top + car.Height >= p24.Top + p24.Height Then
If p24.Visible = True Then
block.Caption = block.Caption - 1
End If
p24.Visible = False
End If
If car.Left <= p25.Left And car.Left + car.Width >= p25.Width + p25.Left And car.Top <= p25.Top And car.Top + car.Height >= p25.Top + p25.Height Then
If p25.Visible = True Then
block.Caption = block.Caption - 1
End If
p25.Visible = False
End If
If car.Left <= p26.Left And car.Left + car.Width >= p26.Width + p26.Left And car.Top <= p26.Top And car.Top + car.Height >= p26.Top + p26.Height Then
If p26.Visible = True Then
block.Caption = block.Caption - 1
End If
p26.Visible = False
End If
If car.Left <= p27.Left And car.Left + car.Width >= p27.Width + p27.Left And car.Top <= p27.Top And car.Top + car.Height >= p27.Top + p27.Height Then
If p27.Visible = True Then
block.Caption = block.Caption - 1
End If
p27.Visible = False
End If
If car.Left <= p28.Left And car.Left + car.Width >= p28.Width + p28.Left And car.Top <= p28.Top And car.Top + car.Height >= p28.Top + p28.Height Then
If p28.Visible = True Then
block.Caption = block.Caption - 1
End If
p28.Visible = False
End If
If car.Left <= p29.Left And car.Left + car.Width >= p29.Width + p29.Left And car.Top <= p29.Top And car.Top + car.Height >= p29.Top + p29.Height Then
If p29.Visible = True Then
block.Caption = block.Caption - 1
End If
p29.Visible = False
End If
If car.Left <= p30.Left And car.Left + car.Width >= p30.Width + p30.Left And car.Top <= p30.Top And car.Top + car.Height >= p30.Top + p30.Height Then
If p30.Visible = True Then
block.Caption = block.Caption - 1
End If
p30.Visible = False
End If
If car.Left <= p31.Left And car.Left + car.Width >= p31.Width + p31.Left And car.Top <= p31.Top And car.Top + car.Height >= p31.Top + p31.Height Then
If p31.Visible = True Then
block.Caption = block.Caption - 1
End If
p31.Visible = False
End If
If car.Left <= p32.Left And car.Left + car.Width >= p32.Width + p32.Left And car.Top <= p32.Top And car.Top + car.Height >= p32.Top + p32.Height Then
If p32.Visible = True Then
block.Caption = block.Caption - 1
End If
p32.Visible = False
End If
If car.Left <= p33.Left And car.Left + car.Width >= p33.Width + p33.Left And car.Top <= p33.Top And car.Top + car.Height >= p33.Top + p33.Height Then
If p33.Visible = True Then
block.Caption = block.Caption - 1
End If
p33.Visible = False
End If
If car.Left <= p34.Left And car.Left + car.Width >= p34.Width + p34.Left And car.Top <= p34.Top And car.Top + car.Height >= p34.Top + p34.Height Then
If p34.Visible = True Then
block.Caption = block.Caption - 1
End If
p34.Visible = False
End If
If car.Left <= p35.Left And car.Left + car.Width >= p35.Width + p35.Left And car.Top <= p35.Top And car.Top + car.Height >= p35.Top + p35.Height Then
If p35.Visible = True Then
block.Caption = block.Caption - 1
End If
p35.Visible = False
End If
If car.Left <= p36.Left And car.Left + car.Width >= p36.Width + p36.Left And car.Top <= p36.Top And car.Top + car.Height >= p36.Top + p36.Height Then
If p36.Visible = True Then
block.Caption = block.Caption - 1
End If
p36.Visible = False
End If
If car.Left <= p37.Left And car.Left + car.Width >= p37.Width + p37.Left And car.Top <= p37.Top And car.Top + car.Height >= p37.Top + p37.Height Then
If p37.Visible = True Then
block.Caption = block.Caption - 1
End If
p37.Visible = False
End If
If car.Left <= p38.Left And car.Left + car.Width >= p38.Width + p38.Left And car.Top <= p38.Top And car.Top + car.Height >= p38.Top + p38.Height Then
If p38.Visible = True Then
block.Caption = block.Caption - 1
End If
p38.Visible = False
End If
If car.Left <= p39.Left And car.Left + car.Width >= p39.Width + p39.Left And car.Top <= p39.Top And car.Top + car.Height >= p39.Top + p39.Height Then
If p39.Visible = True Then
block.Caption = block.Caption - 1
End If
p39.Visible = False
End If
If car.Left <= p40.Left And car.Left + car.Width >= p40.Width + p40.Left And car.Top <= p40.Top And car.Top + car.Height >= p40.Top + p40.Height Then
If p40.Visible = True Then
block.Caption = block.Caption - 1
End If
p40.Visible = False
End If
If car.Left <= p41.Left And car.Left + car.Width >= p41.Width + p41.Left And car.Top <= p41.Top And car.Top + car.Height >= p41.Top + p41.Height Then
If p41.Visible = True Then
block.Caption = block.Caption - 1
End If
p41.Visible = False
End If
If car.Left <= p42.Left And car.Left + car.Width >= p42.Width + p42.Left And car.Top <= p42.Top And car.Top + car.Height >= p42.Top + p42.Height Then
If p42.Visible = True Then
block.Caption = block.Caption - 1
End If
p42.Visible = False
End If
If car.Left <= p43.Left And car.Left + car.Width >= p43.Width + p43.Left And car.Top <= p43.Top And car.Top + car.Height >= p43.Top + p43.Height Then
If p43.Visible = True Then
block.Caption = block.Caption - 1
End If
p43.Visible = False
End If
If car.Left <= p44.Left And car.Left + car.Width >= p44.Width + p44.Left And car.Top <= p44.Top And car.Top + car.Height >= p44.Top + p44.Height Then
If p44.Visible = True Then
block.Caption = block.Caption - 1
End If
p44.Visible = False
End If
If car.Left <= p45.Left And car.Left + car.Width >= p45.Width + p45.Left And car.Top <= p45.Top And car.Top + car.Height >= p45.Top + p45.Height Then
If p45.Visible = True Then
block.Caption = block.Caption - 1
End If
p45.Visible = False
End If
If car.Left <= p46.Left And car.Left + car.Width >= p46.Width + p46.Left And car.Top <= p46.Top And car.Top + car.Height >= p46.Top + p46.Height Then
If p46.Visible = True Then
block.Caption = block.Caption - 1
End If
p46.Visible = False
End If
If car.Left <= p47.Left And car.Left + car.Width >= p47.Width + p47.Left And car.Top <= p47.Top And car.Top + car.Height >= p47.Top + p47.Height Then
If p47.Visible = True Then
block.Caption = block.Caption - 1
End If
p47.Visible = False
End If
If car.Left <= p48.Left And car.Left + car.Width >= p48.Width + p48.Left And car.Top <= p48.Top And car.Top + car.Height >= p48.Top + p48.Height Then
If p48.Visible = True Then
block.Caption = block.Caption - 1
End If
p48.Visible = False
End If
If car.Left <= p49.Left And car.Left + car.Width >= p49.Width + p49.Left And car.Top <= p49.Top And car.Top + car.Height >= p49.Top + p49.Height Then
If p49.Visible = True Then
block.Caption = block.Caption - 1
End If
p49.Visible = False
End If
If car.Left <= p50.Left And car.Left + car.Width >= p50.Width + p50.Left And car.Top <= p50.Top And car.Top + car.Height >= p50.Top + p50.Height Then
If p50.Visible = True Then
block.Caption = block.Caption - 1
End If
p50.Visible = False
End If
If car.Left <= p51.Left And car.Left + car.Width >= p51.Width + p51.Left And car.Top <= p51.Top And car.Top + car.Height >= p51.Top + p51.Height Then
If p51.Visible = True Then
block.Caption = block.Caption - 1
End If
p51.Visible = False
End If
If car.Left <= p52.Left And car.Left + car.Width >= p52.Width + p52.Left And car.Top <= p52.Top And car.Top + car.Height >= p52.Top + p52.Height Then
If p52.Visible = True Then
block.Caption = block.Caption - 1
End If
p52.Visible = False
End If
If car.Left <= p53.Left And car.Left + car.Width >= p53.Width + p53.Left And car.Top <= p53.Top And car.Top + car.Height >= p53.Top + p53.Height Then
If p53.Visible = True Then
block.Caption = block.Caption - 1
End If
p53.Visible = False
End If
If car.Left <= p54.Left And car.Left + car.Width >= p54.Width + p54.Left And car.Top <= p54.Top And car.Top + car.Height >= p54.Top + p54.Height Then
If p54.Visible = True Then
block.Caption = block.Caption - 1
End If
p54.Visible = False
End If
If car.Left <= p55.Left And car.Left + car.Width >= p55.Width + p55.Left And car.Top <= p55.Top And car.Top + car.Height >= p55.Top + p55.Height Then
If p55.Visible = True Then
block.Caption = block.Caption - 1
End If
p55.Visible = False
End If
If car.Left <= p56.Left And car.Left + car.Width >= p56.Width + p56.Left And car.Top <= p56.Top And car.Top + car.Height >= p56.Top + p56.Height Then
If p56.Visible = True Then
block.Caption = block.Caption - 1
End If
p56.Visible = False
End If
If car.Left <= p57.Left And car.Left + car.Width >= p57.Width + p57.Left And car.Top <= p57.Top And car.Top + car.Height >= p57.Top + p57.Height Then
If p57.Visible = True Then
block.Caption = block.Caption - 1
End If
p57.Visible = False
End If
If car.Left <= p58.Left And car.Left + car.Width >= p58.Width + p58.Left And car.Top <= p58.Top And car.Top + car.Height >= p58.Top + p58.Height Then
If p58.Visible = True Then
block.Caption = block.Caption - 1
End If
p58.Visible = False
End If
If car.Left <= p59.Left And car.Left + car.Width >= p59.Width + p59.Left And car.Top <= p59.Top And car.Top + car.Height >= p59.Top + p59.Height Then
If p59.Visible = True Then
block.Caption = block.Caption - 1
End If
p59.Visible = False
End If
If car.Left <= p60.Left And car.Left + car.Width >= p60.Width + p60.Left And car.Top <= p60.Top And car.Top + car.Height >= p60.Top + p60.Height Then
If p60.Visible = True Then
block.Caption = block.Caption - 1
End If
p60.Visible = False
End If
If car.Left <= p61.Left And car.Left + car.Width >= p61.Width + p61.Left And car.Top <= p61.Top And car.Top + car.Height >= p61.Top + p61.Height Then
If p61.Visible = True Then
block.Caption = block.Caption - 1
End If
p61.Visible = False
End If
If car.Left <= p63.Left And car.Left + car.Width >= p63.Width + p63.Left And car.Top <= p63.Top And car.Top + car.Height >= p63.Top + p63.Height Then
If p63.Visible = True Then
block.Caption = block.Caption - 1
End If
p63.Visible = False
End If
If car.Left <= p64.Left And car.Left + car.Width >= p64.Width + p64.Left And car.Top <= p64.Top And car.Top + car.Height >= p64.Top + p64.Height Then
If p64.Visible = True Then
block.Caption = block.Caption - 1
End If
p64.Visible = False
End If
If car.Left <= p65.Left And car.Left + car.Width >= p65.Width + p65.Left And car.Top <= p65.Top And car.Top + car.Height >= p65.Top + p65.Height Then
If p65.Visible = True Then
block.Caption = block.Caption - 1
End If
p65.Visible = False
End If
If car.Left <= p66.Left And car.Left + car.Width >= p66.Width + p66.Left And car.Top <= p66.Top And car.Top + car.Height >= p66.Top + p66.Height Then
If p66.Visible = True Then
block.Caption = block.Caption - 1
End If
p66.Visible = False
End If
If car.Left <= p67.Left And car.Left + car.Width >= p67.Width + p67.Left And car.Top <= p67.Top And car.Top + car.Height >= p67.Top + p67.Height Then
If p67.Visible = True Then
block.Caption = block.Caption - 1
End If
p67.Visible = False
End If
If car.Left <= p68.Left And car.Left + car.Width >= p68.Width + p68.Left And car.Top <= p68.Top And car.Top + car.Height >= p68.Top + p68.Height Then
If p68.Visible = True Then
block.Caption = block.Caption - 1
End If
p68.Visible = False
End If
If car.Left <= p69.Left And car.Left + car.Width >= p69.Width + p69.Left And car.Top <= p69.Top And car.Top + car.Height >= p69.Top + p69.Height Then
If p69.Visible = True Then
block.Caption = block.Caption - 1
End If
p69.Visible = False
End If
If car.Left <= p70.Left And car.Left + car.Width >= p70.Width + p70.Left And car.Top <= p70.Top And car.Top + car.Height >= p70.Top + p70.Height Then
If p70.Visible = True Then
block.Caption = block.Caption - 1
End If
p70.Visible = False
End If
If car.Left <= p71.Left And car.Left + car.Width >= p71.Width + p71.Left And car.Top <= p71.Top And car.Top + car.Height >= p71.Top + p71.Height Then
If p71.Visible = True Then
block.Caption = block.Caption - 1
End If
p71.Visible = False
End If
If car.Left <= p72.Left And car.Left + car.Width >= p72.Width + p72.Left And car.Top <= p72.Top And car.Top + car.Height >= p72.Top + p72.Height Then
If p72.Visible = True Then
block.Caption = block.Caption - 1
End If
p72.Visible = False
End If
If car.Left <= p73.Left And car.Left + car.Width >= p73.Width + p73.Left And car.Top <= p73.Top And car.Top + car.Height >= p73.Top + p73.Height Then
If p73.Visible = True Then
block.Caption = block.Caption - 1
End If
p73.Visible = False
End If
If car.Left <= p74.Left And car.Left + car.Width >= p74.Width + p74.Left And car.Top <= p74.Top And car.Top + car.Height >= p74.Top + p74.Height Then
If p74.Visible = True Then
block.Caption = block.Caption - 1
End If
p74.Visible = False
End If
If car.Left <= p75.Left And car.Left + car.Width >= p75.Width + p75.Left And car.Top <= p75.Top And car.Top + car.Height >= p75.Top + p75.Height Then
If p75.Visible = True Then
block.Caption = block.Caption - 1
End If
p75.Visible = False
End If
If car.Left <= p76.Left And car.Left + car.Width >= p76.Width + p76.Left And car.Top <= p76.Top And car.Top + car.Height >= p76.Top + p76.Height Then
If p76.Visible = True Then
block.Caption = block.Caption - 1
End If
p76.Visible = False
End If
If car.Left <= p78.Left And car.Left + car.Width >= p78.Width + p78.Left And car.Top <= p78.Top And car.Top + car.Height >= p78.Top + p78.Height Then
If p78.Visible = True Then
block.Caption = block.Caption - 1
End If
p78.Visible = False
End If
If car.Left <= p79.Left And car.Left + car.Width >= p79.Width + p79.Left And car.Top <= p79.Top And car.Top + car.Height >= p79.Top + p79.Height Then
If p79.Visible = True Then
block.Caption = block.Caption - 1
End If
p79.Visible = False
End If
If car.Left <= p80.Left And car.Left + car.Width >= p80.Width + p80.Left And car.Top <= p80.Top And car.Top + car.Height >= p80.Top + p80.Height Then
If p80.Visible = True Then
block.Caption = block.Caption - 1
End If
p80.Visible = False
End If
If car.Left <= p81.Left And car.Left + car.Width >= p81.Width + p81.Left And car.Top <= p81.Top And car.Top + car.Height >= p81.Top + p81.Height Then
If p81.Visible = True Then
block.Caption = block.Caption - 1
End If
p81.Visible = False
End If
If car.Left <= p82.Left And car.Left + car.Width >= p82.Width + p82.Left And car.Top <= p82.Top And car.Top + car.Height >= p82.Top + p82.Height Then
If p82.Visible = True Then
block.Caption = block.Caption - 1
End If
p82.Visible = False
End If
If car.Left <= p83.Left And car.Left + car.Width >= p83.Width + p83.Left And car.Top <= p83.Top And car.Top + car.Height >= p83.Top + p83.Height Then
If p83.Visible = True Then
block.Caption = block.Caption - 1
End If
p83.Visible = False
End If
If car.Left <= p84.Left And car.Left + car.Width >= p84.Width + p84.Left And car.Top <= p84.Top And car.Top + car.Height >= p84.Top + p84.Height Then
If p84.Visible = True Then
block.Caption = block.Caption - 1
End If
p84.Visible = False
End If
If car.Left <= p85.Left And car.Left + car.Width >= p85.Width + p85.Left And car.Top <= p85.Top And car.Top + car.Height >= p85.Top + p85.Height Then
If p85.Visible = True Then
block.Caption = block.Caption - 1
End If
p85.Visible = False
End If
If car.Left <= p86.Left And car.Left + car.Width >= p86.Width + p86.Left And car.Top <= p86.Top And car.Top + car.Height >= p86.Top + p86.Height Then
If p86.Visible = True Then
block.Caption = block.Caption - 1
End If
p86.Visible = False
End If
If car.Left <= p87.Left And car.Left + car.Width >= p87.Width + p87.Left And car.Top <= p87.Top And car.Top + car.Height >= p87.Top + p87.Height Then
If p87.Visible = True Then
block.Caption = block.Caption - 1
End If
p87.Visible = False
End If
If car.Left <= p89.Left And car.Left + car.Width >= p89.Width + p89.Left And car.Top <= p89.Top And car.Top + car.Height >= p89.Top + p89.Height Then
If p89.Visible = True Then
block.Caption = block.Caption - 1
End If
p89.Visible = False
End If
If car.Left <= p90.Left And car.Left + car.Width >= p90.Width + p90.Left And car.Top <= p90.Top And car.Top + car.Height >= p90.Top + p90.Height Then
If p90.Visible = True Then
block.Caption = block.Caption - 1
End If
p90.Visible = False
End If
If car.Left <= p91.Left And car.Left + car.Width >= p91.Width + p91.Left And car.Top <= p91.Top And car.Top + car.Height >= p91.Top + p91.Height Then
If p91.Visible = True Then
block.Caption = block.Caption - 1
End If
p91.Visible = False
End If
If car.Left <= p92.Left And car.Left + car.Width >= p92.Width + p92.Left And car.Top <= p92.Top And car.Top + car.Height >= p92.Top + p92.Height Then
If p92.Visible = True Then
block.Caption = block.Caption - 1
End If
p92.Visible = False
End If
If car.Left <= p93.Left And car.Left + car.Width >= p93.Width + p93.Left And car.Top <= p93.Top And car.Top + car.Height >= p93.Top + p93.Height Then
If p93.Visible = True Then
block.Caption = block.Caption - 1
End If
p93.Visible = False
End If
If car.Left <= p94.Left And car.Left + car.Width >= p94.Width + p94.Left And car.Top <= p94.Top And car.Top + car.Height >= p94.Top + p94.Height Then
If p94.Visible = True Then
block.Caption = block.Caption - 1
End If
p94.Visible = False
End If
If car.Left <= p95.Left And car.Left + car.Width >= p95.Width + p95.Left And car.Top <= p95.Top And car.Top + car.Height >= p96.Top + p95.Height Then
If p95.Visible = True Then
block.Caption = block.Caption - 1
End If
p95.Visible = False
End If
If car.Left <= p96.Left And car.Left + car.Width >= p96.Width + p96.Left And car.Top <= p96.Top And car.Top + car.Height >= p96.Top + p96.Height Then
If p96.Visible = True Then
block.Caption = block.Caption - 1
End If
p96.Visible = False
End If
If car.Left <= p97.Left And car.Left + car.Width >= p97.Width + p97.Left And car.Top <= p97.Top And car.Top + car.Height >= p97.Top + p97.Height Then
If p97.Visible = True Then
block.Caption = block.Caption - 1
End If
p97.Visible = False
End If
If car.Left <= p98.Left And car.Left + car.Width >= p98.Width + p98.Left And car.Top <= p98.Top And car.Top + car.Height >= p98.Top + p98.Height Then
If p98.Visible = True Then
block.Caption = block.Caption - 1
End If
p98.Visible = False
End If
If car.Left <= p99.Left And car.Left + car.Width >= p99.Width + p99.Left And car.Top <= p99.Top And car.Top + car.Height >= p99.Top + p99.Height Then
If p99.Visible = True Then
block.Caption = block.Caption - 1
End If
p99.Visible = False
End If
If car.Left <= p100.Left And car.Left + car.Width >= p100.Width + p100.Left And car.Top <= p100.Top And car.Top + car.Height >= p100.Top + p100.Height Then
If p100.Visible = True Then
block.Caption = block.Caption - 1
End If
p100.Visible = False
End If
If car.Left <= p101.Left And car.Left + car.Width >= p101.Width + p101.Left And car.Top <= p101.Top And car.Top + car.Height >= p101.Top + p101.Height Then
If p101.Visible = True Then
block.Caption = block.Caption - 1
End If
p101.Visible = False
End If
If car.Left <= p102.Left And car.Left + car.Width >= p102.Width + p102.Left And car.Top <= p102.Top And car.Top + car.Height >= p102.Top + p102.Height Then
If p102.Visible = True Then
block.Caption = block.Caption - 1
End If
p102.Visible = False
End If
If car.Left <= p103.Left And car.Left + car.Width >= p103.Width + p103.Left And car.Top <= p103.Top And car.Top + car.Height >= p103.Top + p103.Height Then
If p103.Visible = True Then
block.Caption = block.Caption - 1
End If
p103.Visible = False
End If
If car.Left <= p104.Left And car.Left + car.Width >= p104.Width + p104.Left And car.Top <= p104.Top And car.Top + car.Height >= p104.Top + p104.Height Then
If p104.Visible = True Then
block.Caption = block.Caption - 1
End If
p104.Visible = False
End If
If car.Left <= p105.Left And car.Left + car.Width >= p105.Width + p105.Left And car.Top <= p105.Top And car.Top + car.Height >= p105.Top + p105.Height Then
If p105.Visible = True Then
block.Caption = block.Caption - 1
End If
p105.Visible = False
End If
If car.Left <= p106.Left And car.Left + car.Width >= p106.Width + p106.Left And car.Top <= p106.Top And car.Top + car.Height >= p106.Top + p106.Height Then
If p106.Visible = True Then
block.Caption = block.Caption - 1
End If
p106.Visible = False
End If
If car.Left <= p107.Left And car.Left + car.Width >= p107.Width + p107.Left And car.Top <= p107.Top And car.Top + car.Height >= p107.Top + p107.Height Then
If p107.Visible = True Then
block.Caption = block.Caption - 1
End If
p107.Visible = False
End If
If car.Left <= p108.Left And car.Left + car.Width >= p108.Width + p108.Left And car.Top <= p108.Top And car.Top + car.Height >= p108.Top + p108.Height Then
If p108.Visible = True Then
block.Caption = block.Caption - 1
End If
p108.Visible = False
End If
If car.Left <= p109.Left And car.Left + car.Width >= p109.Width + p109.Left And car.Top <= p109.Top And car.Top + car.Height >= p109.Top + p109.Height Then
If p109.Visible = True Then
block.Caption = block.Caption - 1
End If
p109.Visible = False
End If
If car.Left <= p110.Left And car.Left + car.Width >= p110.Width + p110.Left And car.Top <= p110.Top And car.Top + car.Height >= p110.Top + p110.Height Then
If p100.Visible = True Then
block.Caption = block.Caption - 1
End If
p110.Visible = False
End If
If car.Left <= p111.Left And car.Left + car.Width >= p111.Width + p111.Left And car.Top <= p111.Top And car.Top + car.Height >= p111.Top + p111.Height Then
If p111.Visible = True Then
block.Caption = block.Caption - 1
End If
p111.Visible = False
End If
If car.Left <= p112.Left And car.Left + car.Width >= p112.Width + p112.Left And car.Top <= p112.Top And car.Top + car.Height >= p112.Top + p112.Height Then
If p112.Visible = True Then
block.Caption = block.Caption - 1
End If
p112.Visible = False
End If
If car.Left <= p113.Left And car.Left + car.Width >= p113.Width + p113.Left And car.Top <= p113.Top And car.Top + car.Height >= p113.Top + p113.Height Then
If p113.Visible = True Then
block.Caption = block.Caption - 1
End If
p113.Visible = False
End If
If car.Left <= p114.Left And car.Left + car.Width >= p114.Width + p114.Left And car.Top <= p114.Top And car.Top + car.Height >= p114.Top + p114.Height Then
If p114.Visible = True Then
block.Caption = block.Caption - 1
End If
p114.Visible = False
End If
If car.Left <= p115.Left And car.Left + car.Width >= p115.Width + p115.Left And car.Top <= p115.Top And car.Top + car.Height >= p115.Top + p115.Height Then
If p115.Visible = True Then
block.Caption = block.Caption - 1
End If
p115.Visible = False
End If
If car.Left <= p116.Left And car.Left + car.Width >= p116.Width + p116.Left And car.Top <= p116.Top And car.Top + car.Height >= p116.Top + p116.Height Then
If p116.Visible = True Then
block.Caption = block.Caption - 1
End If
p116.Visible = False
End If
If car.Left <= p117.Left And car.Left + car.Width >= p117.Width + p117.Left And car.Top <= p117.Top And car.Top + car.Height >= p117.Top + p117.Height Then
If p117.Visible = True Then
block.Caption = block.Caption - 1
End If
p117.Visible = False
End If
If car.Left <= p118.Left And car.Left + car.Width >= p118.Width + p118.Left And car.Top <= p118.Top And car.Top + car.Height >= p118.Top + p118.Height Then
If p118.Visible = True Then
block.Caption = block.Caption - 1
End If
p118.Visible = False
End If
If car.Left <= p119.Left And car.Left + car.Width >= p119.Width + p119.Left And car.Top <= p119.Top And car.Top + car.Height >= p119.Top + p119.Height Then
If p119.Visible = True Then
block.Caption = block.Caption - 1
End If
p119.Visible = False
End If
If car.Left <= p120.Left And car.Left + car.Width >= p120.Width + p120.Left And car.Top <= p120.Top And car.Top + car.Height >= p120.Top + p120.Height Then
If p120.Visible = True Then
block.Caption = block.Caption - 1
End If
p120.Visible = False
End If
If car.Left <= p121.Left And car.Left + car.Width >= p121.Width + p121.Left And car.Top <= p121.Top And car.Top + car.Height >= p121.Top + p121.Height Then
If p121.Visible = True Then
block.Caption = block.Caption - 1
End If
p121.Visible = False
End If
If car.Left <= p122.Left And car.Left + car.Width >= p122.Width + p122.Left And car.Top <= p122.Top And car.Top + car.Height >= p122.Top + p122.Height Then
If p122.Visible = True Then
block.Caption = block.Caption - 1
End If
p122.Visible = False
End If
If car.Left <= p123.Left And car.Left + car.Width >= p123.Width + p123.Left And car.Top <= p123.Top And car.Top + car.Height >= p123.Top + p123.Height Then
If p123.Visible = True Then
block.Caption = block.Caption - 1
End If
p123.Visible = False
End If
If car.Left <= p124.Left And car.Left + car.Width >= p124.Width + p124.Left And car.Top <= p124.Top And car.Top + car.Height >= p124.Top + p124.Height Then
If p124.Visible = True Then
block.Caption = block.Caption - 1
End If
p124.Visible = False
End If
If car.Left <= p125.Left And car.Left + car.Width >= p125.Width + p125.Left And car.Top <= p125.Top And car.Top + car.Height >= p125.Top + p125.Height Then
If p125.Visible = True Then
block.Caption = block.Caption - 1
End If
p125.Visible = False
End If
If car.Left <= p126.Left And car.Left + car.Width >= p126.Width + p126.Left And car.Top <= p126.Top And car.Top + car.Height >= p126.Top + p126.Height Then
If p126.Visible = True Then
block.Caption = block.Caption - 1
End If
p126.Visible = False
End If
If car.Left <= p127.Left And car.Left + car.Width >= p127.Width + p127.Left And car.Top <= p127.Top And car.Top + car.Height >= p127.Top + p127.Height Then
If p127.Visible = True Then
block.Caption = block.Caption - 1
End If
p127.Visible = False
End If
If car.Left <= p128.Left And car.Left + car.Width >= p128.Width + p128.Left And car.Top <= p128.Top And car.Top + car.Height >= p128.Top + p128.Height Then
If p128.Visible = True Then
block.Caption = block.Caption - 1
End If
p128.Visible = False
End If
If car.Left <= p129.Left And car.Left + car.Width >= p129.Width + p129.Left And car.Top <= p129.Top And car.Top + car.Height >= p129.Top + p129.Height Then
If p129.Visible = True Then
block.Caption = block.Caption - 1
End If
p129.Visible = False
End If
If car.Left <= p130.Left And car.Left + car.Width >= p130.Width + p130.Left And car.Top <= p130.Top And car.Top + car.Height >= p130.Top + p130.Height Then
If p130.Visible = True Then
block.Caption = block.Caption - 1
End If
p130.Visible = False
End If
If car.Left <= p131.Left And car.Left + car.Width >= p131.Width + p131.Left And car.Top <= p131.Top And car.Top + car.Height >= p131.Top + p131.Height Then
If p131.Visible = True Then
block.Caption = block.Caption - 1
End If
p131.Visible = False
End If

'//////////////////\\\\\\\\\\\\\\\\\\\/////////////////\\\\\\\\\\\\\\////////////\\\\\\\\\\\///////////\
If p1.Visible = False And p2.Visible = False And p3.Visible = False And p4.Visible = False And p5.Visible = False And p6.Visible = False And p7.Visible = False And p8.Visible = False And p9.Visible = False And p10.Visible = False And p11.Visible = False And p12.Visible = False And p13.Visible = False And p14.Visible = False And p15.Visible = False And p16.Visible = False And p17.Visible = False And p18.Visible = False And p19.Visible = False And p20.Visible = False And p21.Visible = False And p22.Visible = False And p23.Visible = False And p24.Visible = False And p25.Visible = False And p26.Visible = False And p27.Visible = False And p28.Visible = False And p29.Visible = False And p30.Visible = False And p31.Visible = False And p32.Visible = False And p33.Visible = False And p34.Visible = False And p35.Visible = False And p36.Visible = False And p37.Visible = False And p38.Visible = False And p39.Visible = False And p40.Visible = False And p41.Visible = False And p42.Visible = False Then
Let dog = dog + 1
End If
If p44.Visible = False And p45.Visible = False And p46.Visible = False And p47.Visible = False And p48.Visible = False And p49.Visible = False And p50.Visible = False And p51.Visible = False And p52.Visible = False And p53.Visible = False And p54.Visible = False And p55.Visible = False And p56.Visible = False And p57.Visible = False And p58.Visible = False And p59.Visible = False And p60.Visible = False And p61.Visible = False And p63.Visible = False And p64.Visible = False And p65.Visible = False And p66.Visible = False And p67.Visible = False And p68.Visible = False And p69.Visible = False And p70.Visible = False And p71.Visible = False And p72.Visible = False And p73.Visible = False And p74.Visible = False And p75.Visible = False And p76.Visible = False And p78.Visible = False And p79.Visible = False And p80.Visible = False And p81.Visible = False And p82.Visible = False And p83.Visible = False And p84.Visible = False And p85.Visible = False Then
Let cat = cat + 1
End If
If p43.Visible = False And p86.Visible = False And p87.Visible = False And p89.Visible = False And p90.Visible = False And p91.Visible = False And p92.Visible = False And p93.Visible = False And p94.Visible = False And p95.Visible = False And p96.Visible = False And p97.Visible = False And p98.Visible = False And p99.Visible = False And p100.Visible = False And p101.Visible = False And p102.Visible = False And p103.Visible = False And p104.Visible = False And p105.Visible = False And p106.Visible = False And p107.Visible = False And p108.Visible = False And p109.Visible = False And p110.Visible = False And p111.Visible = False And p112.Visible = False And p113.Visible = False And p114.Visible = False And p115.Visible = False And p116.Visible = False And p117.Visible = False And p118.Visible = False And p119.Visible = False And p120.Visible = False And p121.Visible = False And p122.Visible = False And p123.Visible = False And p124.Visible = False Then
Let fish = fish + 1
End If
If p125.Visible = False And p126.Visible = False And p127.Visible = False And p128.Visible = False And p129.Visible = False And p130.Visible = False And p131.Visible = False Then
Let cow = cow + 1
End If
Label1.Caption = cow & " " & cat & " " & dog & " " & fish
'///////////////\/\/\/\/\/\/\/\/\/\/\/\/\////////////////\/\//\/\/\\\\\\\\\\





'///////////\\\\\\\\\\\\\\\\\\\\/\//\//\/\/\/\//\/\/\/\/\/\/\/\/\/\//\/\/\/\

If dog = 1 And fish = 1 And cat = 1 And cow = 1 Then
tmrKey.Enabled = False
Timer1.Enabled = False
smokescreen.Visible = True
p1.Visible = False
p2.Visible = False
p3.Visible = False
p4.Visible = False
p5.Visible = False
p6.Visible = False
p7.Visible = False
p8.Visible = False
p9.Visible = False
p10.Visible = False
p11.Visible = False
p12.Visible = False
p13.Visible = False
p14.Visible = False
p15.Visible = False
p16.Visible = False
p17.Visible = False
p18.Visible = False
p19.Visible = False
p20.Visible = False
p21.Visible = False
p22.Visible = False
p23.Visible = False
p24.Visible = False
p25.Visible = False
p26.Visible = False
p27.Visible = False
p28.Visible = False
p29.Visible = False
p30.Visible = False
p31.Visible = False
p32.Visible = False
p33.Visible = False
p34.Visible = False
p35.Visible = False
p36.Visible = False
p37.Visible = False
p38.Visible = False
p39.Visible = False
p40.Visible = False
p41.Visible = False
p42.Visible = False
p43.Visible = False
p44.Visible = False
p45.Visible = False
p46.Visible = False
p47.Visible = False
p48.Visible = False
p49.Visible = False
p60.Visible = False
p50.Visible = False
p51.Visible = False
p52.Visible = False
p53.Visible = False
p54.Visible = False
p55.Visible = False
p56.Visible = False
p57.Visible = False
p58.Visible = False
p59.Visible = False
p61.Visible = False
p63.Visible = False
p64.Visible = False
p65.Visible = False
p66.Visible = False
p67.Visible = False
p68.Visible = False
p69.Visible = False
p80.Visible = False
p70.Visible = False
p71.Visible = False
p72.Visible = False
p73.Visible = False
p74.Visible = False
p75.Visible = False
p76.Visible = False
p78.Visible = False
p79.Visible = False
p82.Visible = False
p83.Visible = False
p84.Visible = False
p85.Visible = False
p86.Visible = False
p87.Visible = False
p89.Visible = False
p81.Visible = False
p91.Visible = False
p92.Visible = False
p93.Visible = False
p94.Visible = False
p95.Visible = False
p96.Visible = False
p97.Visible = False
p98.Visible = False
p99.Visible = False
p90.Visible = False
p101.Visible = False
p102.Visible = False
p103.Visible = False
p104.Visible = False
p105.Visible = False
p106.Visible = False
p107.Visible = False
p108.Visible = False
p109.Visible = False
p100.Visible = False
p110.Visible = False
p111.Visible = False
p112.Visible = False
p113.Visible = False
p114.Visible = False
p115.Visible = False
p116.Visible = False
p117.Visible = False
p118.Visible = False
p119.Visible = False
p120.Visible = False
p121.Visible = False
p122.Visible = False
p123.Visible = False
p124.Visible = False
p125.Visible = False
p126.Visible = False
p127.Visible = False
p128.Visible = False
p129.Visible = False
p130.Visible = False
p131.Visible = False
nextlevel
End If
'\\\\\\\\\\\\\\\\\\\99999999\\\\\\\\\\\\\\\\999999999999\\\\\\\\





If iKeyCounter = 203 Then
car.Left = car.Left - cleft
End If
If iKeyCounter = 205 Then
car.Left = car.Left + cright

End If
If iKeyCounter = 200 Then
car.Top = car.Top - cup

End If
If iKeyCounter = 208 Then
car.Top = car.Top + cdown
End If
                  
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
     End If
     
     
     
     
     
     
     Next
     
   
     DoEvents  'Very Very Important. You need to give DirectX time to process what you've asked it to do, If you dont it'll
 'crash.
End Sub

Private Sub hitandrun()
p1.Visible = False
p2.Visible = False
p3.Visible = False
p4.Visible = False
p5.Visible = False
p6.Visible = False
p7.Visible = False
p8.Visible = False
p9.Visible = False
p10.Visible = False
p11.Visible = False
p12.Visible = False
p13.Visible = False
p14.Visible = False
p15.Visible = False
p16.Visible = False
p17.Visible = False
p18.Visible = False
p19.Visible = False
p20.Visible = False
p21.Visible = False
p22.Visible = False
p23.Visible = False
p24.Visible = False
p25.Visible = False
p26.Visible = False
p27.Visible = False
p28.Visible = False
p29.Visible = False
p30.Visible = False
p31.Visible = False
p32.Visible = False
p33.Visible = False
p34.Visible = False
p35.Visible = False
p36.Visible = False
p37.Visible = False
p38.Visible = False
p39.Visible = False
p40.Visible = False
p41.Visible = False
p42.Visible = False
p43.Visible = False
p44.Visible = False
p45.Visible = False
p46.Visible = False
p47.Visible = False
p48.Visible = False
p49.Visible = False
p60.Visible = False
p50.Visible = False
p51.Visible = False
p52.Visible = False
p53.Visible = False
p54.Visible = False
p55.Visible = False
p56.Visible = False
p57.Visible = False
p58.Visible = False
p59.Visible = False
p61.Visible = False
p63.Visible = False
p64.Visible = False
p65.Visible = False
p66.Visible = False
p67.Visible = False
p68.Visible = False
p69.Visible = False
p80.Visible = False
p70.Visible = False
p71.Visible = False
p72.Visible = False
p73.Visible = False
p74.Visible = False
p75.Visible = False
p76.Visible = False
p78.Visible = False
p79.Visible = False
p82.Visible = False
p83.Visible = False
p84.Visible = False
p85.Visible = False
p86.Visible = False
p87.Visible = False
p89.Visible = False
p81.Visible = False
p91.Visible = False
p92.Visible = False
p93.Visible = False
p94.Visible = False
p95.Visible = False
p96.Visible = False
p97.Visible = False
p98.Visible = False
p99.Visible = False
p90.Visible = False
p101.Visible = False
p102.Visible = False
p103.Visible = False
p104.Visible = False
p105.Visible = False
p106.Visible = False
p107.Visible = False
p108.Visible = False
p109.Visible = False
p100.Visible = False
p110.Visible = False
p111.Visible = False
p112.Visible = False
p113.Visible = False
p114.Visible = False
p115.Visible = False
p116.Visible = False
p117.Visible = False
p118.Visible = False
p119.Visible = False
p120.Visible = False
p121.Visible = False
p122.Visible = False
p123.Visible = False
p124.Visible = False
p125.Visible = False
p126.Visible = False
p127.Visible = False
p128.Visible = False
p129.Visible = False
p130.Visible = False
p131.Visible = False
If level.Caption = "ONE" Then
block.Caption = "48"
cup = 0
cdown = 0
cleft = 0
cright = 0
Let speed = 10
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "TWO" Then
block.Caption = "63"
cup = 0
cdown = 0
cleft = 0
cright = 0
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "THREE" Then
block.Caption = "78"
cup = 0
cdown = 0
cleft = 0
cright = 0
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
p25.Visible = True
p33.Visible = True
p34.Visible = True
p41.Visible = True
p49.Visible = True
p50.Visible = True
p100.Visible = True
p124.Visible = True
p125.Visible = True
p116.Visible = True
p108.Visible = True
p109.Visible = True
p57.Visible = True
p66.Visible = True
p67.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "FOUR" Then
Let speed = 20
block.Caption = "67"
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p25.Visible = True
p26.Visible = True
p27.Visible = True
p36.Visible = True
p44.Visible = True
p53.Visible = True
p104.Visible = True
p129.Visible = True
p121.Visible = True
p114.Visible = True
p64.Visible = True
p73.Visible = True
p82.Visible = True
p99.Visible = True
p91.Visible = True
p81.Visible = True
p72.Visible = True
p63.Visible = True
p113.Visible = True
p120.Visible = True
p128.Visible = True
p103.Visible = True
p52.Visible = True
p43.Visible = True
p35.Visible = True
p34.Visible = True
p42.Visible = True
p51.Visible = True
p102.Visible = True
p127.Visible = True
p119.Visible = True
p112.Visible = True
p61.Visible = True
p71.Visible = True
p90.Visible = True
p98.Visible = True
p16.Visible = True
p24.Visible = True
p31.Visible = True
p39.Visible = True
p45.Visible = True
p46.Visible = True
p54.Visible = True
p111.Visible = True
p59.Visible = True
p68.Visible = True
p75.Visible = True
p84.Visible = True
p92.Visible = True
p80.Visible = True
p83.Visible = True
p74.Visible = True
p67.Visible = True
p58.Visible = True
p110.Visible = True
p118.Visible = True
p38.Visible = True
p30.Visible = True
p23.Visible = True
p14.Visible = True
p15.Visible = True
Timer2.Enabled = True
End If


If level.Caption = "FIVE" Then
block.Caption = "90"
p7.Visible = True
p9.Visible = True
p6.Visible = True
p8.Visible = True
p10.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p26.Visible = True
p27.Visible = True
p28.Visible = True
p29.Visible = True
p30.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p41.Visible = True
p42.Visible = True
p43.Visible = True
p44.Visible = True
p45.Visible = True
p46.Visible = True
p47.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p100.Visible = True
p101.Visible = True
p102.Visible = True
p103.Visible = True
p104.Visible = True
p105.Visible = True
p106.Visible = True
p107.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p116.Visible = True
p117.Visible = True
p118.Visible = True
p119.Visible = True
p120.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p58.Visible = True
p59.Visible = True
p60.Visible = True
p61.Visible = True
p63.Visible = True
p64.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p95.Visible = True
p96.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "SIX" Then
block.Caption = "96"
p1.Visible = True
p3.Visible = True
p2.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p23.Visible = True
p24.Visible = True
p26.Visible = True
p27.Visible = True
p28.Visible = True
p29.Visible = True
p30.Visible = True
p31.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p41.Visible = True
p42.Visible = True
p43.Visible = True
p44.Visible = True
p45.Visible = True
p46.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p100.Visible = True
p101.Visible = True
p102.Visible = True
p103.Visible = True
p104.Visible = True
p105.Visible = True
p106.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p117.Visible = True
p118.Visible = True
p119.Visible = True
p120.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p58.Visible = True
p59.Visible = True
p60.Visible = True
p61.Visible = True
p63.Visible = True
p64.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p74.Visible = True
p75.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p87.Visible = True
p89.Visible = True
p91.Visible = True
p92.Visible = True
p96.Visible = True
p98.Visible = True
p99.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "SEVEN" Then
block.Caption = "63"
p1.Visible = True
p3.Visible = True
p5.Visible = True
p7.Visible = True
p9.Visible = True
p11.Visible = True
p13.Visible = True
p15.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p33.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p40.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p108.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p66.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p73.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "EIGHT" Then
block.Caption = "64"
p1.Visible = True
p3.Visible = True
p5.Visible = True
p7.Visible = True
p9.Visible = True
p11.Visible = True
p13.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p32.Visible = True
p33.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p48.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p107.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p123.Visible = True
p108.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p65.Visible = True
p66.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p82.Visible = True
p83.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "NINE" Then
block.Caption = "127"
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
p25.Visible = True
p33.Visible = True
p34.Visible = True
p41.Visible = True
p49.Visible = True
p50.Visible = True
p100.Visible = True
p124.Visible = True
p125.Visible = True
p116.Visible = True
p108.Visible = True
p109.Visible = True
p57.Visible = True
p66.Visible = True
p67.Visible = True
p26.Visible = True
p27.Visible = True
p30.Visible = True
p31.Visible = True
p32.Visible = True
p35.Visible = True
p36.Visible = True
p38.Visible = True
p39.Visible = True
p40.Visible = True
p42.Visible = True
p43.Visible = True
p46.Visible = True
p47.Visible = True
p48.Visible = True
p51.Visible = True
p52.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p58.Visible = True
p59.Visible = True
p63.Visible = True
p64.Visible = True
p65.Visible = True
p68.Visible = True
p69.Visible = True
p71.Visible = True
p72.Visible = True
p73.Visible = True
p101.Visible = True
p102.Visible = True
p105.Visible = True
p106.Visible = True
p107.Visible = True
p110.Visible = True
p111.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p117.Visible = True
p118.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p126.Visible = True
p127.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
Timer2.Enabled = True
End If
If level.Caption = "TEN" Then
p15.Visible = True
p16.Visible = True
p91.Visible = True
p82.Visible = True
p98.Visible = True
p99.Visible = True
p53.Visible = True
p104.Visible = True
p128.Visible = True
p103.Visible = True
p1.Visible = True
p2.Visible = True
p3.Visible = True
p17.Visible = True
block.Caption = "14"
Timer2.Enabled = True
End If

End Sub

Private Sub nextlevel()
p1.Visible = False
p2.Visible = False
p3.Visible = False
p4.Visible = False
p5.Visible = False
p6.Visible = False
p7.Visible = False
p8.Visible = False
p9.Visible = False
p10.Visible = False
p11.Visible = False
p12.Visible = False
p13.Visible = False
p14.Visible = False
p15.Visible = False
p16.Visible = False
p17.Visible = False
p18.Visible = False
p19.Visible = False
p20.Visible = False
p21.Visible = False
p22.Visible = False
p23.Visible = False
p24.Visible = False
p25.Visible = False
p26.Visible = False
p27.Visible = False
p28.Visible = False
p29.Visible = False
p30.Visible = False
p31.Visible = False
p32.Visible = False
p33.Visible = False
p34.Visible = False
p35.Visible = False
p36.Visible = False
p37.Visible = False
p38.Visible = False
p39.Visible = False
p40.Visible = False
p41.Visible = False
p42.Visible = False
p43.Visible = False
p44.Visible = False
p45.Visible = False
p46.Visible = False
p47.Visible = False
p48.Visible = False
p49.Visible = False
p60.Visible = False
p50.Visible = False
p51.Visible = False
p52.Visible = False
p53.Visible = False
p54.Visible = False
p55.Visible = False
p56.Visible = False
p57.Visible = False
p58.Visible = False
p59.Visible = False
p61.Visible = False
p63.Visible = False
p64.Visible = False
p65.Visible = False
p66.Visible = False
p67.Visible = False
p68.Visible = False
p69.Visible = False
p80.Visible = False
p70.Visible = False
p71.Visible = False
p72.Visible = False
p73.Visible = False
p74.Visible = False
p75.Visible = False
p76.Visible = False
p78.Visible = False
p79.Visible = False
p82.Visible = False
p83.Visible = False
p84.Visible = False
p85.Visible = False
p86.Visible = False
p87.Visible = False
p89.Visible = False
p81.Visible = False
p91.Visible = False
p92.Visible = False
p93.Visible = False
p94.Visible = False
p95.Visible = False
p96.Visible = False
p97.Visible = False
p98.Visible = False
p99.Visible = False
p90.Visible = False
p101.Visible = False
p102.Visible = False
p103.Visible = False
p104.Visible = False
p105.Visible = False
p106.Visible = False
p107.Visible = False
p108.Visible = False
p109.Visible = False
p100.Visible = False
p110.Visible = False
p111.Visible = False
p112.Visible = False
p113.Visible = False
p114.Visible = False
p115.Visible = False
p116.Visible = False
p117.Visible = False
p118.Visible = False
p119.Visible = False
p120.Visible = False
p121.Visible = False
p122.Visible = False
p123.Visible = False
p124.Visible = False
p125.Visible = False
p126.Visible = False
p127.Visible = False
p128.Visible = False
p129.Visible = False
p130.Visible = False
p131.Visible = False
Timer1.Enabled = False
counter.Caption = "5"
tmrKey.Enabled = False
ready.Visible = True
ready2.Visible = True
If level.Caption = "" Then
ready2.Caption = "1"
level.Caption = "ONE"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "ONE" Then
ready2.Caption = "2"
level.Caption = "TWO"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "TWO" Then
ready2.Caption = "3"
level.Caption = "THREE"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "THREE" Then
ready2.Caption = "4"
level.Caption = "FOUR"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "FOUR" Then
ready2.Caption = "5"
level.Caption = "FIVE"
hitandrun
Exit Sub
End If
If level.Caption = "FIVE" Then
ready2.Caption = "6"
level.Caption = "SIX"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "SIX" Then
ready2.Caption = "7"
level.Caption = "SEVEN"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "SEVEN" Then
ready2.Caption = "8"
level.Caption = "EIGHT"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "EIGHT" Then
ready2.Caption = "9"
level.Caption = "NINE"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "NINE" Then
ready2.Caption = "10"
level.Caption = "TEN"
Timer1.Enabled = False
hitandrun
Exit Sub
End If
If level.Caption = "TEN" Then
ready.Visible = False
ready2.Visible = False
smokescreen.Visible = False
newgame
Exit Sub
End If

End Sub

Private Sub resert()
car.Left = cage.Left
opp.Left = cage.Left + cage.Width - opp.Width
lives.Caption = lives.Caption - 1
If lives.Caption = "0" Then
newgame
End If
If level.Caption = "" Then
level.Caption = "ONE"
opp.Top = cage.Top
opp.Left = p15.Left
car.Top = p83.Top
car.Left = cage.Left
End If
p1.Visible = False
p2.Visible = False
p3.Visible = False
p4.Visible = False
p5.Visible = False
p6.Visible = False
p7.Visible = False
p8.Visible = False
p9.Visible = False
p10.Visible = False
p11.Visible = False
p12.Visible = False
p13.Visible = False
p14.Visible = False
p15.Visible = False
p16.Visible = False
p17.Visible = False
p18.Visible = False
p19.Visible = False
p20.Visible = False
p21.Visible = False
p22.Visible = False
p23.Visible = False
p24.Visible = False
p25.Visible = False
p26.Visible = False
p27.Visible = False
p28.Visible = False
p29.Visible = False
p30.Visible = False
p31.Visible = False
p32.Visible = False
p33.Visible = False
p34.Visible = False
p35.Visible = False
p36.Visible = False
p37.Visible = False
p38.Visible = False
p39.Visible = False
p40.Visible = False
p41.Visible = False
p42.Visible = False
p43.Visible = False
p44.Visible = False
p45.Visible = False
p46.Visible = False
p47.Visible = False
p48.Visible = False
p49.Visible = False
p60.Visible = False
p50.Visible = False
p51.Visible = False
p52.Visible = False
p53.Visible = False
p54.Visible = False
p55.Visible = False
p56.Visible = False
p57.Visible = False
p58.Visible = False
p59.Visible = False
p61.Visible = False
p63.Visible = False
p64.Visible = False
p65.Visible = False
p66.Visible = False
p67.Visible = False
p68.Visible = False
p69.Visible = False
p80.Visible = False
p70.Visible = False
p71.Visible = False
p72.Visible = False
p73.Visible = False
p74.Visible = False
p75.Visible = False
p76.Visible = False
p78.Visible = False
p79.Visible = False
p82.Visible = False
p83.Visible = False
p84.Visible = False
p85.Visible = False
p86.Visible = False
p87.Visible = False
p89.Visible = False
p81.Visible = False
p91.Visible = False
p92.Visible = False
p93.Visible = False
p94.Visible = False
p95.Visible = False
p96.Visible = False
p97.Visible = False
p98.Visible = False
p99.Visible = False
p90.Visible = False
p101.Visible = False
p102.Visible = False
p103.Visible = False
p104.Visible = False
p105.Visible = False
p106.Visible = False
p107.Visible = False
p108.Visible = False
p109.Visible = False
p100.Visible = False
p110.Visible = False
p111.Visible = False
p112.Visible = False
p113.Visible = False
p114.Visible = False
p115.Visible = False
p116.Visible = False
p117.Visible = False
p118.Visible = False
p119.Visible = False
p120.Visible = False
p121.Visible = False
p122.Visible = False
p123.Visible = False
p124.Visible = False
p125.Visible = False
p126.Visible = False
p127.Visible = False
p128.Visible = False
p129.Visible = False
p130.Visible = False
p131.Visible = False
If level.Caption = "ONE" Then
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
block.Caption = "48"
End If
If level.Caption = "TWO" Then
block.Caption = "63"
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
End If
If level.Caption = "THREE" Then
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
p25.Visible = True
p33.Visible = True
p34.Visible = True
p41.Visible = True
p49.Visible = True
p50.Visible = True
p100.Visible = True
p124.Visible = True
p125.Visible = True
p116.Visible = True
p108.Visible = True
p109.Visible = True
p57.Visible = True
p66.Visible = True
p67.Visible = True
block.Caption = "78"
End If
If level.Caption = "FOUR" Then
block.Caption = "65"
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p25.Visible = True
p26.Visible = True
p27.Visible = True
p36.Visible = True
p44.Visible = True
p53.Visible = True
p104.Visible = True
p129.Visible = True
p121.Visible = True
p114.Visible = True
p64.Visible = True
p73.Visible = True
p82.Visible = True
p99.Visible = True
p91.Visible = True
p81.Visible = True
p72.Visible = True
p63.Visible = True
p113.Visible = True
p120.Visible = True
p128.Visible = True
p103.Visible = True
p52.Visible = True
p43.Visible = True
p35.Visible = True
p34.Visible = True
p42.Visible = True
p51.Visible = True
p102.Visible = True
p127.Visible = True
p119.Visible = True
p112.Visible = True
p61.Visible = True
p71.Visible = True
p90.Visible = True
p98.Visible = True
p16.Visible = True
p24.Visible = True
p31.Visible = True
p39.Visible = True
p45.Visible = True
p46.Visible = True
p54.Visible = True
p111.Visible = True
p59.Visible = True
p68.Visible = True
p75.Visible = True
p84.Visible = True
p92.Visible = True
p80.Visible = True
p83.Visible = True
p74.Visible = True
p67.Visible = True
p58.Visible = True
p110.Visible = True
p118.Visible = True
p38.Visible = True
p30.Visible = True
p23.Visible = True
p14.Visible = True
p15.Visible = True
End If
If level.Caption = "FIVE" Then
block.Caption = "85"
p7.Visible = True
p9.Visible = True
p6.Visible = True
p8.Visible = True
p10.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p26.Visible = True
p27.Visible = True
p28.Visible = True
p29.Visible = True
p30.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p41.Visible = True
p42.Visible = True
p43.Visible = True
p44.Visible = True
p45.Visible = True
p46.Visible = True
p47.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p100.Visible = True
p101.Visible = True
p102.Visible = True
p103.Visible = True
p104.Visible = True
p105.Visible = True
p106.Visible = True
p107.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p116.Visible = True
p117.Visible = True
p118.Visible = True
p119.Visible = True
p120.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p58.Visible = True
p59.Visible = True
p60.Visible = True
p61.Visible = True
p63.Visible = True
p64.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p95.Visible = True
p96.Visible = True
End If
If level.Caption = "SIX" Then
block.Caption = "93"
p1.Visible = True
p3.Visible = True
p2.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p23.Visible = True
p24.Visible = True
p26.Visible = True
p27.Visible = True
p28.Visible = True
p29.Visible = True
p30.Visible = True
p31.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p41.Visible = True
p42.Visible = True
p43.Visible = True
p44.Visible = True
p45.Visible = True
p46.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p100.Visible = True
p101.Visible = True
p102.Visible = True
p103.Visible = True
p104.Visible = True
p105.Visible = True
p106.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p117.Visible = True
p118.Visible = True
p119.Visible = True
p120.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p58.Visible = True
p59.Visible = True
p60.Visible = True
p61.Visible = True
p63.Visible = True
p64.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p74.Visible = True
p75.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p87.Visible = True
p89.Visible = True
p91.Visible = True
p92.Visible = True
p96.Visible = True
p98.Visible = True
p99.Visible = True
End If
If level.Caption = "SEVEN" Then
p1.Visible = True
p3.Visible = True
p5.Visible = True
p7.Visible = True
p9.Visible = True
p11.Visible = True
p13.Visible = True
p15.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p33.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p40.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
p108.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p66.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p73.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
End If
If level.Caption = "EIGHT" Then
block.Caption = "64"
p1.Visible = True
p3.Visible = True
p5.Visible = True
p7.Visible = True
p9.Visible = True
p11.Visible = True
p13.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p32.Visible = True
p33.Visible = True
p34.Visible = True
p35.Visible = True
p36.Visible = True
p37.Visible = True
p38.Visible = True
p39.Visible = True
p48.Visible = True
p49.Visible = True
p50.Visible = True
p51.Visible = True
p52.Visible = True
p53.Visible = True
p54.Visible = True
p55.Visible = True
p107.Visible = True
p124.Visible = True
p125.Visible = True
p126.Visible = True
p127.Visible = True
p128.Visible = True
p129.Visible = True
p130.Visible = True
p123.Visible = True
p108.Visible = True
p109.Visible = True
p110.Visible = True
p111.Visible = True
p112.Visible = True
p113.Visible = True
p114.Visible = True
p65.Visible = True
p66.Visible = True
p67.Visible = True
p68.Visible = True
p69.Visible = True
p70.Visible = True
p71.Visible = True
p72.Visible = True
p82.Visible = True
p83.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
End If
If level.Caption = "NINE" Then
p1.Visible = True
p2.Visible = True
p3.Visible = True
p4.Visible = True
p5.Visible = True
p6.Visible = True
p7.Visible = True
p8.Visible = True
p9.Visible = True
p10.Visible = True
p11.Visible = True
p12.Visible = True
p13.Visible = True
p14.Visible = True
p15.Visible = True
p16.Visible = True
p17.Visible = True
p18.Visible = True
p19.Visible = True
p20.Visible = True
p21.Visible = True
p22.Visible = True
p23.Visible = True
p24.Visible = True
p74.Visible = True
p75.Visible = True
p76.Visible = True
p78.Visible = True
p79.Visible = True
p80.Visible = True
p81.Visible = True
p82.Visible = True
p83.Visible = True
p84.Visible = True
p85.Visible = True
p86.Visible = True
p87.Visible = True
p89.Visible = True
p90.Visible = True
p91.Visible = True
p92.Visible = True
p93.Visible = True
p94.Visible = True
p95.Visible = True
p96.Visible = True
p97.Visible = True
p98.Visible = True
p99.Visible = True
p28.Visible = True
p29.Visible = True
p37.Visible = True
p44.Visible = True
p45.Visible = True
p53.Visible = True
p103.Visible = True
p104.Visible = True
p128.Visible = True
p119.Visible = True
p120.Visible = True
p112.Visible = True
p60.Visible = True
p61.Visible = True
p70.Visible = True
p25.Visible = True
p33.Visible = True
p34.Visible = True
p41.Visible = True
p49.Visible = True
p50.Visible = True
p100.Visible = True
p124.Visible = True
p125.Visible = True
p116.Visible = True
p108.Visible = True
p109.Visible = True
p57.Visible = True
p66.Visible = True
p67.Visible = True
p26.Visible = True
p27.Visible = True
p30.Visible = True
p31.Visible = True
p32.Visible = True
p35.Visible = True
p36.Visible = True
p38.Visible = True
p39.Visible = True
p40.Visible = True
p42.Visible = True
p43.Visible = True
p46.Visible = True
p47.Visible = True
p48.Visible = True
p51.Visible = True
p52.Visible = True
p54.Visible = True
p55.Visible = True
p56.Visible = True
p58.Visible = True
p59.Visible = True
p63.Visible = True
p64.Visible = True
p65.Visible = True
p68.Visible = True
p69.Visible = True
p71.Visible = True
p72.Visible = True
p73.Visible = True
p101.Visible = True
p102.Visible = True
p105.Visible = True
p106.Visible = True
p107.Visible = True
p110.Visible = True
p111.Visible = True
p113.Visible = True
p114.Visible = True
p115.Visible = True
p117.Visible = True
p118.Visible = True
p121.Visible = True
p122.Visible = True
p123.Visible = True
p126.Visible = True
p127.Visible = True
p129.Visible = True
p130.Visible = True
p131.Visible = True
End If
If level.Caption = "TEN" Then
p15.Visible = True
p16.Visible = True
p91.Visible = True
p82.Visible = True
p98.Visible = True
p99.Visible = True
p53.Visible = True
p104.Visible = True
p128.Visible = True
p103.Visible = True
p1.Visible = True
p2.Visible = True
p3.Visible = True
p17.Visible = True
block.Caption = "14"
End If
End Sub
Private Sub newgame()
p1.Visible = False
p2.Visible = False
p3.Visible = False
p4.Visible = False
p5.Visible = False
p6.Visible = False
p7.Visible = False
p8.Visible = False
p9.Visible = False
p10.Visible = False
p11.Visible = False
p12.Visible = False
p13.Visible = False
p14.Visible = False
p15.Visible = False
p16.Visible = False
p17.Visible = False
p18.Visible = False
p19.Visible = False
p20.Visible = False
p21.Visible = False
p22.Visible = False
p23.Visible = False
p24.Visible = False
p25.Visible = False
p26.Visible = False
p27.Visible = False
p28.Visible = False
p29.Visible = False
p30.Visible = False
p31.Visible = False
p32.Visible = False
p33.Visible = False
p34.Visible = False
p35.Visible = False
p36.Visible = False
p37.Visible = False
p38.Visible = False
p39.Visible = False
p40.Visible = False
p41.Visible = False
p42.Visible = False
p43.Visible = False
p44.Visible = False
p45.Visible = False
p46.Visible = False
p47.Visible = False
p48.Visible = False
p49.Visible = False
p60.Visible = False
p50.Visible = False
p51.Visible = False
p52.Visible = False
p53.Visible = False
p54.Visible = False
p55.Visible = False
p56.Visible = False
p57.Visible = False
p58.Visible = False
p59.Visible = False
p61.Visible = False
p63.Visible = False
p64.Visible = False
p65.Visible = False
p66.Visible = False
p67.Visible = False
p68.Visible = False
p69.Visible = False
p80.Visible = False
p70.Visible = False
p71.Visible = False
p72.Visible = False
p73.Visible = False
p74.Visible = False
p75.Visible = False
p76.Visible = False
p78.Visible = False
p79.Visible = False
p82.Visible = False
p83.Visible = False
p84.Visible = False
p85.Visible = False
p86.Visible = False
p87.Visible = False
p89.Visible = False
p81.Visible = False
p91.Visible = False
p92.Visible = False
p93.Visible = False
p94.Visible = False
p95.Visible = False
p96.Visible = False
p97.Visible = False
p98.Visible = False
p99.Visible = False
p90.Visible = False
p101.Visible = False
p102.Visible = False
p103.Visible = False
p104.Visible = False
p105.Visible = False
p106.Visible = False
p107.Visible = False
p108.Visible = False
p109.Visible = False
p100.Visible = False
p110.Visible = False
p111.Visible = False
p112.Visible = False
p113.Visible = False
p114.Visible = False
p115.Visible = False
p116.Visible = False
p117.Visible = False
p118.Visible = False
p119.Visible = False
p120.Visible = False
p121.Visible = False
p122.Visible = False
p123.Visible = False
p124.Visible = False
p125.Visible = False
p126.Visible = False
p127.Visible = False
p128.Visible = False
p129.Visible = False
p130.Visible = False
p131.Visible = False
stmenu.Height = 5895
stmenu.Width = 7455
Timer1.Enabled = False
Timer2.Enabled = False
tmrKey.Enabled = False
level.Caption = ""
liveblock.Visible = False
End Sub
