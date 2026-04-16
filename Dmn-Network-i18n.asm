;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                   (default application texts [english])                    @
;@                                                                            @
;@             (c) 2015-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;### POINTER ##################################################################

prgtxtoky   db 1:dw prgtxtoky_eng
prgtxtcnc   db 1:dw prgtxtcnc_eng

stamentxt1   db 1:dw stamentxt1_eng
stamentxt2   db 1:dw stamentxt2_eng
stamentxt11_poi   db 1:dw stamentxt11_eng
stamentxt12_poi   db 1:dw stamentxt12_eng
stamentxt13_poi   db 1:dw stamentxt13_eng
stamentxt21_poi   db 1:dw stamentxt21_eng
stamentxt22_poi   db 1:dw stamentxt22_eng

statxttit   db 1:dw statxttit_eng
statxtbta   db 1:dw statxtbta_eng
statxtbtb   db 1:dw statxtbtb_eng
statxtbtc   db 1:dw statxtbtc_eng

statxttba1   db 1:dw statxttba1_eng
statxttba3   db 1:dw statxttba3_eng

statxtfra   db 1:dw statxtfra_eng

statxttxc   db 1:dw statxttxc_eng
statxttxd   db 1:dw statxttxd_eng
statxttxe   db 1:dw statxttxe_eng
statxttxf   db 1:dw statxttxf_eng
statxttxg   db 1:dw statxttxg_eng

statxttxh   db 1:dw statxttxh_eng
statxttxh1   db 1:dw statxttxh1_eng

statxtfrb   db 1:dw statxtfrb_eng
statxtfrc   db 1:dw statxtfrc_eng

statxtfrd   db 1:dw statxtfrd_eng
statxtfre   db 1:dw statxtfre_eng
statxtfrf   db 1:dw statxtfrf_eng
statxtfrg   db 1:dw statxtfrg_eng
statxtfrh   db 1:dw statxtfrh_eng
statxtfri   db 1:dw statxtfri_eng

;config dialogue text data
cfgtxttit   db 1:dw cfgtxttit_eng

cfgtxttba1   db 1:dw cfgtxttba1_eng
cfgtxttba2   db 1:dw cfgtxttba2_eng

cfgtxttxa   db 1:dw cfgtxttxa_eng
cfgtxttxb   db 1:dw cfgtxttxb_eng
cfgtxttxc   db 1:dw cfgtxttxc_eng
cfgtxttxd   db 1:dw cfgtxttxd_eng
cfgtxttxf   db 1:dw cfgtxttxf_eng
cfgtxttxg   db 1:dw cfgtxttxg_eng
cfgtxttxh   db 1:dw cfgtxttxh_eng

cfgtxttxi   db 1:dw cfgtxttxi_eng
cfgtxttxj   db 1:dw cfgtxttxj_eng
cfgtxttxk   db 1:dw cfgtxttxk_eng

cfgtxtrda   db 1:dw cfgtxtrda_eng
cfgtxtrdb   db 1:dw cfgtxtrdb_eng
cfgtxtrdc   db 1:dw cfgtxtrdc_eng
cfgtxtrdd   db 1:dw cfgtxtrdd_eng

cfgerrtxt0   db 1:dw cfgerrtxt0_eng
cfgerrtxt1   db 1:dw cfgerrtxt1_eng
cfgerrtxt2   db 1:dw cfgerrtxt2_eng


;### TEXTS ####################################################################

prgtxtoky_eng   db "Ok",0
prgtxtcnc_eng   db "Cancel",0

stamentxt1_eng  db "File",0
stamentxt2_eng  db "?",0
stamentxt11_eng   db " Hide on startup",0
stamentxt12_eng   db " Reset all connections",0
stamentxt13_eng   db " Quit",0
stamentxt21_eng   db " Index",0
stamentxt22_eng   db " About",0

statxttit_eng   db "Network daemon",0
statxtbta_eng   db "Hide",0
statxtbtb_eng   db "Network settings",0
statxtbtc_eng   db "Apply",0

statxttba1_eng  db "Status",0
statxttba3_eng  db "Driver",0

statxtfra_eng   db "Network status",0

statxttxc_eng   db "Status",0
statxttxd_eng   db "Connections",0
statxttxe_eng   db "Maximum",0
statxttxf_eng   db "Sent",0
statxttxg_eng   db "Received",0

statxttxh_eng   db "Type",0
statxttxh1_eng  db "Manually set",0

statxtfrb_eng   db "IP settings",0
statxtfrc_eng   db "DNS settings",0

statxtfrd_eng   db "Status",0
statxtfre_eng   db "DenYoNet Settings",0
statxtfrf_eng   db "Localhost Settings",0
statxtfrg_eng   db "GR8NET Settings",0
statxtfrh_eng   db "M4 Board Settings",0
statxtfri_eng   db "Net4CPC Settings",0

;config dialogue text data
cfgtxttit_eng   db "TCP/IP properties",0

cfgtxttba1_eng  db "IP address",0
cfgtxttba2_eng  db "DNS & Hostname",0

cfgtxttxa_eng   db "You can get IP settings assigned automatically",0
cfgtxttxb_eng   db "if your network supports this capability.",0
cfgtxttxc_eng   db "Otherwise, you need to ask your network admin",0
cfgtxttxd_eng   db "for the appropriate IP settings.",0

cfgtxttxf_eng   db "IP address",0
cfgtxttxg_eng   db "Subnet mask",0
cfgtxttxh_eng   db "Default gateway",0

cfgtxttxi_eng   db "Hostname",0
cfgtxttxj_eng   db "Primary DNS",0
cfgtxttxk_eng   db "Secondary DNS",0

cfgtxtrda_eng   db "Obtain an IP address automatically",0
cfgtxtrdb_eng   db "Use the following IP address ",0
cfgtxtrdc_eng   db "Obtain DNS server addresses automatically",0
cfgtxtrdd_eng   db "Use the following DNS server addresses ",0

cfgerrtxt0_eng  db "Wrong IP format",0
cfgerrtxt1_eng  db "Please enter a correct",0
cfgerrtxt2_eng  db "number between 0 and 255.",0

;### RESERVE
ds 80
