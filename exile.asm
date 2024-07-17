read_xauthority_cookie:
	mov rsi, [rsp]          ; get number of command line args
	lea rsi, [rsp+rsi*8+16] ; get address of first environment var
	lea rbx, [xauth]        ; "XAUTHORITY" string
next_variable:
	mov rcx, [rsi]          ; tested env variable
	test ecx, ecx           ; zero means end of env var array
	jz open_connection      ; if there is no $XAUTHORITY go straight to opening a connection
	add rsi, 8
	xor edx, edx            ; loop counter
next_char:
	mov al, [rcx+rdx]       ; read character from tested var
	cmp al, [rbx+rdx]       ; compare it to the xauth string
	jne next_variable
	inc edx
	cmp edx, 11             ; reached end of xauth string?
	jne next_char
found_path:
	lea rdi, [rcx+rdx]
	mov eax, 2 ; open
	xor esi, esi
	syscall

	mov ebx, eax

	mov eax, 17 ; pread
	mov edi, ebx
	lea rsi, [conn_request_cookie]
	mov edx, 16
	mov r10d, 38
	syscall

	mov eax, 3 ; close
	mov edi, ebx
	syscall

open_connection:
; create socket
mov eax, 41  ; socket
mov edi, 1   ; AF_UNIX
mov esi, 1   ; SOCK_STREAM
xor edx, edx ; protocol
syscall

; store sockfd
mov r12d, eax

; connect to socket
mov eax, 42   ; connect
mov edi, r12d ; sockfd
lea rsi, [sockaddr]
mov edx, 110
syscall

; write connection request
mov eax, 1
mov edi, r12d
lea rsi, [conn_request]
mov edx, 48
syscall

; read connection request reply
sub rsp, 8276 ; TODO: should get this from the response
xor eax, eax
mov edi, r12d
mov rsi, rsp
mov edx, 8276
syscall

; zero first byte of the reply indicates connection failure
cmp m8 [rsp], 0
je exit_error

; store gc id
mov eax, [rsp+12]
mov [create_gc+4], eax
mov [put_image_gc], eax

; store window id
inc eax
mov [create_window_id], eax
mov [map_window+4], eax
mov [put_image_window], eax
mov [change_wm_class_window], eax
mov [change_wm_name_window], eax
mov [change_wm_protocols_window], eax

; store window root
movzx ebx, m16 [rsp+24] ; read 'length of vendor' ; TODO: align to multiples of 4
movzx ecx, m8 [rsp+29]  ; read 'number of formats'
lea rbx, [rbx+rcx*8]    ; skip past 'vendor' and 'formats'
mov eax, [rsp+rbx+40]
mov [create_window_parent], eax
mov [create_gc+8], eax

; query big-requests extension
mov eax, 1
mov edi, r12d
lea rsi, [query_big_requests]
mov edx, 20
syscall

; read big-requests extension reply
xor eax, eax
mov edi, r12d
mov rsi, rsp
mov edx, 32
syscall

mov al, [rsp+9]
mov [big_req_enable], al

; enable big requests
mov eax, 1
mov edi, r12d
lea rsi, [big_req_enable]
mov edx, 4
syscall

; have to read reply here
xor eax, eax
mov edi, r12d
mov rsi, rsp
mov edx, 32
syscall

; write create window request
mov eax, 1
mov edi, r12d
lea rsi, [create_window]
mov edx, 36
syscall

; write change wm class request
mov eax, 1
mov edi, r12d
lea rsi, [change_wm_class]
mov edx, 36
syscall

; write change wm name request
mov eax, 1
mov edi, r12d
lea rsi, [change_wm_name]
mov edx, 32
syscall

; intern WM_PROTOCOLS atom
mov eax, 1
mov edi, r12d
lea rsi, [intern_atom_wm_protocols]
mov edx, 20
syscall
xor eax, eax
mov edi, r12d
mov rsi, rsp
mov edx, 32
syscall
movzx eax, m16 [rsp+8]
mov [change_wm_protocols_property], eax

; intern WM_DELETE_WINDOW atom
mov eax, 1
mov edi, r12d
lea rsi, [intern_atom_wm_delete_window]
mov edx, 24
syscall
xor eax, eax
mov edi, r12d
mov rsi, rsp
mov edx, 32
syscall
movzx eax, m16 [rsp+8]
mov [change_wm_protocols_value], eax

; change window WM_PROTOCOLS property
mov eax, 1
mov edi, r12d
lea rsi, [change_wm_protocols]
mov edx, 28
syscall

; write create gc request
mov eax, 1
mov edi, r12d
lea rsi, [create_gc]
mov edx, 16
syscall

; write map window request
mov eax, 1
mov edi, r12d
lea rsi, [map_window]
mov edx, 8
syscall

add rsp, 8276

call draw_tilemap
call draw_char

main_loop:
; read event
	xor eax, eax
	mov edi, r12d
	mov rsi, rsp
	mov edx, 32
	syscall
	movzx eax, m8 [rsp] ; load event id
	cmp eax, 2 ; keypress event?
	je process_keydown
	cmp eax, 12 ; expose event?
	je refresh_screen
	cmp eax, 161 ; what is this event?
	jne main_loop
	mov eax, [rsp+8]
	mov ebx, [change_wm_protocols_property]
	cmp eax, ebx
	jne main_loop
	mov eax, [rsp+12]
	mov ebx, [change_wm_protocols_value]
	cmp eax, ebx
	jne main_loop
	jmp exit

refresh_screen:
	; write put image request
	mov eax, 1
	mov edi, r12d
	lea rsi, [put_image]
	mov edx, 691228
	syscall
	jmp main_loop

process_keydown:
	movzx eax, m8 [rsp+1] ; read keycode
	cmp eax, 0x18 ; is it 'Q'
	je exit
	cmp eax, 0x72 ; is it right arrow key
	je process_keydown__move_right
	cmp eax, 0x2e ; is it 'l' key
	je process_keydown__move_right
	cmp eax, 0x74 ; is it down arrow key
	je process_keydown__move_down
	cmp eax, 0x2c ; is it 'j' key
	je process_keydown__move_down
	cmp eax, 0x71 ; is it left arrow key
	je process_keydown__move_left
	cmp eax, 0x2b ; is it 'h' key
	je process_keydown__move_left
	cmp eax, 0x6f ; is it up arrow key
	je process_keydown__move_up
	cmp eax, 0x2d ; is it 'k' key
	je process_keydown__move_up
	jmp main_loop
process_keydown__move_right:
	mov ebx, 1
	xor ecx, ecx
	call move_char
	jmp refresh_screen
process_keydown__move_down:
	xor ebx, ebx
	mov ecx, 1
	call move_char
	jmp refresh_screen
process_keydown__move_left:
	mov ebx, -1
	xor ecx, ecx
	call move_char
	jmp refresh_screen
process_keydown__move_up:
	xor ebx, ebx
	mov ecx, -1
	call move_char
	jmp refresh_screen

exit_error:
	mov eax, 60
	mov edi, 1
	syscall

exit:
	mov eax, 60
	xor edi, edi
	syscall

draw_char:
	lea rsi, [char]
	lea rdi, [screen]
	movzx eax, m8 [char_x]
	movzx ebx, m8 [char_y]
	imul eax, eax, 96
	imul ebx, ebx, 46080
	add rdi, rax
	add rdi, rbx
	mov ecx, 9  ; 16-bit value counter
	mov edx, 12 ; column counter
draw_char_0:
	movzx eax, m16 [rsi]
	mov ebx, 16 ; bit count
draw_char_1:
	test eax, 1
	jz draw_char_2
	mov m32 [rdi], 0xffffffff
	mov m32 [rdi+4], 0xffffffff
	mov m32 [rdi+1920], 0xffffffff
	mov m32 [rdi+1920+4], 0xffffffff
draw_char_2:
	add rdi, 8
	dec edx
	jnz draw_char_3
	add rdi, 3744 ; move down 2 lines in the screen buffer
	mov edx, 12
draw_char_3:
	shr eax, 1
	dec ebx
	jnz draw_char_1
	add rsi, 2
	dec ecx
	jnz draw_char_0
	ret

; @ebx: x coord
; @ecx: y coord
draw_tile:
	; get offset to the screen buffer
	imul ebx, ebx, 96
	imul ecx, ecx, 46080
	lea rdi, [screen]
	add rdi, rbx
	add rdi, rcx
	; move to the center of the tile
	add rdi, 23088
	; draw a dot (2x2 pixels)
	mov eax, 0x666666
	mov [rdi], eax
	mov [rdi+4], eax
	mov [rdi+1920], eax
	mov [rdi+1920+4], eax
	ret

draw_tilemap:
	push r8
	push r9
	push r10
	lea r8, [tilemap]
	xor r10d, r10d ; y counter
draw_tilemap__row:
	xor r9d, r9d ; x counter
draw_tilemap__column:
	movzx eax, m8 [r8]
	inc r8
	cmp eax, 1
	jne draw_tilemap__no_tile
	mov ebx, r9d
	mov ecx, r10d
	call draw_tile
draw_tilemap__no_tile:
	inc r9d
	cmp r9d, 20
	jne draw_tilemap__column
	inc r10d
	cmp r10d, 15
	jne draw_tilemap__row
	pop r10
	pop r9
	pop r8
	ret

; @ebx: tile x
; @ecx: tile y
clear_tile:
	lea rdi, [screen]
	imul ebx, ebx, 96
	imul ecx, ecx, 46080
	add rdi, rbx
	add rdi, rcx
	mov ebx, 24 ; y counter
clear_cell_row:
	xor eax, eax ; x counter
clear_cell_column:
	mov m32 [rdi+rax*4], 0
	inc eax
	cmp eax, 24
	jne clear_cell_column
	add rdi, 1920
	dec ebx
	jnz clear_cell_row
	ret

; @ebx: delta x
; @ecx: delta y
move_char:
	push r8
	push r9
	; store original coords for later use
	movzx r8d, m8 [char_x]
	movzx r9d, m8 [char_y]
	; compute new coords
	add ebx, r8d
	add ecx, r9d
	; get offset into the tilemap for the new coords
	imul eax, ecx, 20
	add eax, ebx
	; check if the tile is walkable
	lea rsi, [tilemap]
	cmp m8 [rsi+rax], 0
	je move_char__done
	; store new character coords
	mov [char_x], bl
	mov [char_y], cl
	; clear the previously occupied tile
	mov ebx, r8d
	mov ecx, r9d
	call clear_tile
	; get offset into the tilemap for the old coords
	imul eax, r9d, 20
	add eax, r8d
	; if the tile has a sprite, redraw it
	lea rsi, [tilemap]
	cmp m8 [rsi+rax], 1
	jne move_char__draw_char
	mov ebx, r8d
	mov ecx, r9d
	call draw_tile
move_char__draw_char:
	call draw_char
move_char__done:
	pop r9
	pop r8
	ret

xauth:
	.i8 "XAUTHORITY="

sockaddr:
	.i8 1 0
	.i8 "/tmp/.X11-unix/X0"
	.res 91

conn_request:
	.i8 108 0                    ; ASCII 'l' to specify little endian
	.i16 11                      ; major version
	.i16 0                       ; minor version
	.i16 18                      ; auth protocol name length
	.i16 16                      ; auth protocol data length
	.i8 0 0                      ; pad
	.i8 "MIT-MAGIC-COOKIE-1" 0 0 ; auth protocol name
conn_request_cookie:
	.res 16

create_window:
	.i8 1                ; opcode
	.i8 0                ; depth
	.i16 9               ; length
create_window_id:
	.i32 0               ; window id
create_window_parent:
	.i32 0               ; parent id
	.i16 0 0 480 360 ; x, y, width, height
	.i16 0 1             ; border, class
	.i32 0               ; visual
	.i32 0x00000800      ; event-mask
	.i32 0x00008001      ; keypress + exposure

map_window:
	.i8 8 0 2 0
	.i32 0

create_gc:
	.i8 55 0  ; opcode
	.i16 4    ; length
	.i32 0    ; id
	.i32 0    ; drawable
	.i32 0    ; mask

query_big_requests:
	.i8 98 0
	.i16 5
	.i16 12
	.i16 0
	.i8 "BIG-REQUESTS"

big_req_enable:
	.i8 0 0 1 0

change_wm_class:
	.i8 18 0                   ; opcode, "replace"
	.i16 9                     ; length
change_wm_class_window:
	.i32 0                     ; window id
	.i32 67 31                 ; property (WM_CLASS), type (STRING)
	.i32 8                     ; format
	.i32  11                   ; length
	.i8 "exile" 0 "exile" 0

change_wm_name:
	.i8 18 0 ; opcode, "replace"
	.i16 8 ; length
change_wm_name_window:
	.i32 0 ; window id
	.i32 39 31 ; property (WM_NAME), type (STRING)
	.i32 8 ; format
	.i32 5 ; length
	.i8 "exile" 0 0 0

change_wm_protocols:
	.i8 18 0 ; opcode, "replace"
	.i16 7 ; length
change_wm_protocols_window:
	.i32 0 ; window id
change_wm_protocols_property:
	.i32 0 
	.i32 4 ; type (ATOM)
	.i32 32 ; format
	.i32 1 ; length
change_wm_protocols_value:
	.i32 0

intern_atom_wm_protocols:
	.i8 16 0 5 0
	.i32 12; length of name
	.i8 "WM_PROTOCOLS"

intern_atom_wm_delete_window:
	.i8 16 0 6 0
	.i32 16; length of name
	.i8 "WM_DELETE_WINDOW"

char:
	.i16 0x0000 0xe000 0x1f00 0x0160 0xc012 0x0100 0xc138 0x1833 0x0002

char_x:
	.i8 3
char_y:
	.i8 2

tilemap:
	.i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0
	.i8 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0
	.i8 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 0 0
	.i8 0 0 1 1 1 0 0 0 0 1 1 0 0 0 0 1 1 1 0 0
	.i8 0 0 1 1 1 0 0 0 0 1 0 0 0 1 1 1 1 1 0 0
	.i8 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0
	.i8 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
	.i8 0 0 0 0 1 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0
	.i8 0 0 0 0 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	.i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

put_image:
	.i8 72           ; opcode
	.i8 2            ; format (ZPixmap)
	.i16 0           ; length (0 to allow big request)
	.i32 172807      ; actual length
put_image_window:
	.i32 0           ; window id
put_image_gc:
	.i32 0           ; gc id
	.i16 480 360 0 0 ; width, height,x, y
	.i8 0 24 0 0     ; left-pad, depth
screen:
	.res 691200
