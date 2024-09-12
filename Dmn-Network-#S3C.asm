nolist

DRIVER      equ 3    ;0=localhost, 1=denyonet, 2=gr8net, 3=m4cpc, 4=sf3cpc
low_buflen  equ 0

org #1000

WRITE "f:\symbos\netd-s3c.exe"
READ "..\..\..\SRC-Main\SymbOS-Constants.asm"
READ "Dmn-Network-head.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-SystemManager.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-DesktopManager.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-FileManager.asm"
READ "Dmn-Network-SF3.asm"
READ "Dmn-Network.asm"
READ "Dmn-Network-SF3CPC.asm"

App_EndTrns

relocate_table
relocate_end
