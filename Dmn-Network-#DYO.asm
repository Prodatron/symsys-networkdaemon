nolist

DRIVER      equ 1    ;0=localhost, 1=denyonet, 2=gr8net, 3=m4cpc
low_buflen  equ 0

org #1000

WRITE "f:\symbos\netd-dyo.exe"
READ "..\..\..\SRC-Main\SymbOS-Constants.asm"
READ "Dmn-Network-head.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-SystemManager.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-DesktopManager.asm"
READ "..\..\..\SRC-Main\Docs-Developer\symbos_lib-FileManager.asm"
READ "Dmn-Network-W5100.asm"
READ "Dmn-Network.asm"
READ "Dmn-Network-W5100DYO.asm"

App_EndTrns

relocate_table
relocate_end
