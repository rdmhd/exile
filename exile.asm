read_xauthority_cookie:
  mov rsi, [rsp]          ; get number of command line args
  lea rsi, [rsp+rsi*8+16] ; get address of first environment var
  lea rbx, [xauth]        ; "XAUTHORITY" string
: mov rcx, [rsi]          ; tested env variable
  test ecx, ecx           ; zero means end of env var array
  jz open_connection      ; if there is no $XAUTHORITY go straight to opening a connection
  add rsi, 8
  xor edx, edx            ; loop counter
: mov al, [rcx+rdx]       ; read character from tested var
  cmp al, [rbx+rdx]       ; compare it to the xauth string
  jne <<
  inc edx
  cmp edx, 11             ; reached end of xauth string?
  jne <

  lea rdi, [rcx+rdx]
  mov eax, 2 ; open
  xor esi, esi
  syscall

  mov ebx, eax

  mov eax, 17 ; pread
  mov edi, ebx
  lea rsi, [conn_request.cookie]
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
mov [put_image.gc], eax

; store window id
inc eax
mov [create_window.id], eax
mov [map_window+4], eax
mov [put_image.window], eax
mov [change_wm_class.window], eax
mov [change_wm_name.window], eax
mov [change_wm_protocols.window], eax

; store window root
movzx ebx, m16 [rsp+24] ; read 'length of vendor' ; TODO: align to multiples of 4
movzx ecx, m8 [rsp+29]  ; read 'number of formats'
lea rbx, [rbx+rcx*8]    ; skip past 'vendor' and 'formats'
mov eax, [rsp+rbx+40]
mov [create_window.parent], eax
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
mov [change_wm_protocols.property], eax

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
mov [change_wm_protocols.value], eax

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

lea rax, [sockfd]
mov [rax], r12d

; seed rng
rdrand eax
mov [rng_state], eax

call generate_map
call draw_world

main_loop:
; read event
  lea rax, [sockfd]
  mov edi, [rax]
  xor eax, eax
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
  mov ebx, [change_wm_protocols.property]
  cmp eax, ebx
  jne main_loop
  mov eax, [rsp+12]
  mov ebx, [change_wm_protocols.value]
  cmp eax, ebx
  jne main_loop
  jmp exit

refresh_screen:
  ; write put image request
  lea rax, [sockfd]
  mov edi, [rax]
  mov eax, 1
  lea rsi, [put_image]
  mov edx, (7 + 480 * 360) * 4
  syscall
  jmp main_loop

process_keydown:
  movzx eax, m8 [rsp+1] ; read keycode
  cmp eax, 0x18 ; is it 'Q'
  je exit
  cmp eax, 0x72 ; is it right arrow key
  je .move_right
  cmp eax, 0x2e ; is it 'l' key
  je .move_right
  cmp eax, 0x74 ; is it down arrow key
  je .move_down
  cmp eax, 0x2c ; is it 'j' key
  je .move_down
  cmp eax, 0x71 ; is it left arrow key
  je .move_left
  cmp eax, 0x2b ; is it 'h' key
  je .move_left
  cmp eax, 0x6f ; is it up arrow key
  je .move_up
  cmp eax, 0x2d ; is it 'k' key
  je .move_up
  cmp eax, 0x1b ; is it 'r' key
  jne main_loop
  call clear_map
  call generate_map
  call clear_screen
  call draw_world
  jmp refresh_screen
.move_right:
  mov ebx, 1
  xor ecx, ecx
  call move_char
  test eax, eax
  jz main_loop
  jmp refresh_screen
.move_down:
  xor ebx, ebx
  mov ecx, 1
  call move_char
  test eax, eax
  jz main_loop
  jmp refresh_screen
.move_left:
  mov ebx, -1
  xor ecx, ecx
  call move_char
  test eax, eax
  jz main_loop
  jmp refresh_screen
.move_up:
  xor ebx, ebx
  mov ecx, -1
  call move_char
  test eax, eax
  jz main_loop
  jmp refresh_screen

exit_error:
  mov eax, 60
  mov edi, 1
  syscall

exit:
  mov eax, 60
  xor edi, edi
  syscall

; x: eax, y: ebx, sprite: ecx
draw_sprite:
  push r8
  push r9

  lea rdx, [screen]
  imul eax, eax, 24 * 4
  imul ebx, ebx, 24 * 4 * 480
  add rdx, rax
  add rdx, rbx

  imul edi, ecx, 9 * 4
  lea rcx, [sprites]
  add rcx, rdi

  xor r8d, r8d ; val counter
  xor r9d, r9d ; x counter

: mov eax, [rcx+r8*4]
  mov ebx, 16
: mov ebp, eax
  and ebp, 0x3
  jz >
  mov esi, 0xffffff
  mov edi, 0x000000
  cmp ebp, 0x2
  cmove esi, edi
  mov m32 [rdx], esi
  mov m32 [rdx+4], esi
  mov m32 [rdx+1920], esi
  mov m32 [rdx+1920+4], esi
: inc r9d
  cmp r9d, 12
  jne >
  add rdx, (480 * 2 - 24) * 4
  xor r9d, r9d
: add rdx, 8
  shr eax, 2
  dec ebx
  jnz <<<
  inc r8d
  cmp r8d, 9
  jl <<<<

  pop r9
  pop r8
  ret

; entity id: eax
draw_entity:
  push r10
  mov r10d, eax
  lea rax, [entities]
  lea r10, [rax+r10*4]
  movzx eax, m8 [r10+0]
  movzx ebx, m8 [r10+1]
  movzx ecx, m8 [r10+2]
  call draw_sprite
  cmp m8 [r10+3], 0
  je >
  movzx ebx, m8 [r10+0]
  movzx ecx, m8 [r10+1]
  mov edx, 0xff0000
  call draw_point
: pop r10
  ret

; sprite: eax, x: ebx, y: ecx
draw_tile:
  push r8
  lea rsi, [tiles-9*2]
  lea rdi, [screen]
  imul ebx, ebx, 24 * 4
  imul ecx, ecx, 24 * 4 * 480
  imul eax, eax, 9 * 2
  add rdi, rbx
  add rdi, rcx
  add rsi, rax
  mov ecx, 9  ; 16-bit value counter
  mov edx, 12 ; column counter
: movzx eax, m16 [rsi]
  mov ebx, 16 ; bit count
: xor r8d, r8d
  mov ebp, 0xffffff
  test eax, 1
  cmovnz r8d, ebp
  mov m32 [rdi], r8d
  mov m32 [rdi+4], r8d
  mov m32 [rdi+1920], r8d
  mov m32 [rdi+1920+4], r8d
  add rdi, 8
  dec edx
  jnz >
  add rdi, (480 * 2 - 24) * 4 ; move down 2 lines in the screen buffer
  mov edx, 12
: shr eax, 1
  dec ebx
  jnz <<
  add rsi, 2
  dec ecx
  jnz <<<
  pop r8
  ret

draw_tilemap:
  push r8
  push r9
  push r10
  lea r8, [tilemap]
  xor r10d, r10d ; y counter
: xor r9d, r9d ; x counter
: movzx eax, m8 [r8]
  inc r8
  and eax, 0x3
  jz >
  mov ebx, r9d
  mov ecx, r10d
  call draw_tile
: inc r9d
  cmp r9d, 20
  jne <<
  inc r10d
  cmp r10d, 15
  jne <<<
  pop r10
  pop r9
  pop r8
  ret

redraw_tilemap:
  push r8
  push r9
  push r10
  push r11
  lea r8, [tilemap]
  xor r10d, r10d ; y counter
: xor r9d, r9d ; x counter
: movzx r11d, m8 [r8]
  test r11b, 0x8 ; is the 'redraw' flag set?
  jz >
  mov eax, r11d
  and eax, 0x3
  mov ebx, r9d
  mov ecx, r10d
  call draw_tile
  test r11b, 0x70 ; is there entity on the tile?
  jz >
  mov eax, r11d
  shr eax, 4
  and eax, 0x7
  call draw_entity
  and m8 [r8], 0xf7 ; remove 'redraw' flag
: inc r8
  inc r9d
  cmp r9d, 20
  jne <<
  inc r10d
  cmp r10d, 15
  jne <<<
  pop r11
  pop r10
  pop r9
  pop r8
  ret

draw_world:
  push r8
  push r9
  push r10
  call draw_tilemap

  lea r8, [entities]
  mov r9d, 1

: mov eax, r9d
  call draw_entity
  inc r9d
  cmp r9d, 5
  jl <

  pop r10
  pop r9
  pop r8
  ret

clear_screen:
  ; TODO: rep stosb?
  lea rax, [screen]
  xor ebx, ebx
: mov m64 [rax+rbx*8], 0
  inc ebx
  cmp ebx, 86400
  jl <
  ret

; @ebx: delta x
; @ecx: delta y
; -> if moved: eax
move_char:
  mov edx, 1
  call move_entity
  test ebx, ebx
  jz >
  ; TODO: attack here
  jmp >>
: test eax, eax
  jz >>
: call simulate_entities
  call redraw_tilemap
  mov eax, 1
: ret

; delta x: ebx, delta y: ecx, id: edx
; -> entity moved: eax, blocking entity: ebx
move_entity:
  push r8
  push r9
  push r10
  xor eax, eax
  ; store original coords for later use
  imul esi, edx, 4
  lea r10, [entities]
  add r10, rsi
  movzx r8d, m8 [r10+0]
  movzx r9d, m8 [r10+1]
  ; compute new coords
  add ebx, r8d
  add ecx, r9d
  ; get offset into the tilemap for the new coords
  imul edi, ecx, 20
  add edi, ebx
  ; check if the tile is walkable
  lea rsi, [tilemap]
  test m8 [rsi+rdi], 0x80
  jz >

  ; check if the tile is occupied by another entity
  ; TODO: this is probably only useful to char movement
  mov ebp, ebx
  movzx ebx, m8 [rsi+rdi]
  and ebx, 0x70
  shr ebx, 4
  test ebx, ebx
  jnz >
  mov ebx, ebp

  ; store new entity coords
  mov [r10+0], bl
  mov [r10+1], cl

  movzx eax, m8 [rsi+rdi]
  ; add entity id to tile
  shl edx, 4
  or eax, edx
  ; set 'redraw' flag on the tile
  or al, 0x8
  mov [rsi+rdi], al

  ; get offset into the tilemap for the old coords
  imul edi, r9d, 20
  add edi, r8d
  movzx eax, m8 [rsi+rdi]
  ; remove entity id from tile
  and al, 0x8f
  ; set 'redraw' flag on the tile
  or al, 0x8
  mov [rsi+rdi], al

  mov eax, 1
: pop r10
  pop r9
  pop r8
  ret

simulate_entities:
  push r8
  push r9
  mov r8d, 2
  lea r9, [entities]
: mov ebx, r8d
  call move_randomly

  movzx eax, m8 [r9+r8*4+0]
  movzx ebx, m8 [r9+r8*4+1]
  call char_visible
  mov bl, [r9+r8*4+3]
  mov [r9+r8*4+3], al
  cmp al, bl
  je >

  movzx eax, m8 [r9+r8*4+0]
  movzx ebx, m8 [r9+r8*4+1]
  imul ebx, ebx, 20
  lea rcx, [tilemap]
  add rcx, rax
  add rcx, rbx
  or m8 [rcx], 0x8

: inc r8d
  cmp r8d, 5
  jl <<
  pop r9
  pop r8
  ret

; entity id: ebx
move_randomly:
  mov eax, 4
  call random_int
  cmp eax, 0
  jne >
  mov ebx, 1
  xor ecx, ecx
  jmp .move
: cmp eax, 1
  jne >
  mov ebx, -1
  xor ecx, ecx
  jmp .move
: cmp eax, 2
  jne >
  xor ebx, ebx
  mov ecx, 1
  jmp .move
: cmp eax, 3
  xor ebx, ebx
  mov ecx, -1
  jmp .move
.move:
  mov edx, r8d
  call move_entity
  ret

; x0: eax, y0: ebx, x1: ecx, y1: edx, maxdist: esi
; -> hit char: eax
bresenham:
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14

  mov r13d, esi
  xor r14d, r14d

  mov esi, 1
  mov edi, 18
  ; clamp x0 to map bounds
  cmp eax, esi
  cmovl eax, esi
  cmp eax, edi
  cmovg eax, edi
  ; clamp x1 to map bounds
  cmp ecx, esi
  cmovl ecx, esi
  cmp ecx, edi
  cmovg ecx, edi

  mov esi, 1
  mov edi, 13
  ; clamp y0 to map bounds
  cmp ebx, esi
  cmovl ebx, esi
  cmp ebx, edi
  cmovg ebx, edi
  ; clamp y1 to map bounds
  cmp edx, esi
  cmovl edx, esi
  cmp edx, edi
  cmovg edx, edi

  mov r8d, ecx ; x1
  mov r9d, edx ; y1

  ; [esi] dx = abs(x1 - x0)
  mov ecx, r8d
  sub ecx, eax
  mov ebp, ecx
  sar ebp, 31
  mov esi, ecx
  add esi, ebp
  xor esi, ebp
  ; [ecx] sx = sign(x1 - x0)
  sar ecx, 31
  or ecx, 1
  ; [edi] dy = -abs(y1 - y0)
  mov edx, r9d
  sub edx, ebx
  mov ebp, edx
  sar ebp, 31
  mov edi, edx
  add edi, ebp
  xor edi, ebp
  neg edi
  ; [edx] sy = sign(y1 - y0)
  sar edx, 31
  or edx, 1
  ; [ebp] e = dx + dy
  mov ebp, esi
  add ebp, edi

  xor r10d, r10d
: inc r10d
  ; don't continue if the tile is a wall
  lea r11, [tilemap]
  imul r12d, ebx, 20
  add r11, rax
  add r11, r12
  movzx r12d, m8 [r11]
  cmp r12b, 3
  je .done
  ; hit char?
  and r12b, 0x70
  cmp r12b, 0x10
  sete r14b
  je .done
  ; compute e2
  mov r11d, ebp
  add r11d, ebp
  ; if e2 >= dy
  cmp r11d, edi
  jl >
  cmp eax, r8d
  je .done
  add ebp, edi
  add eax, ecx
  dec r13d
  ; if e <= dx
: cmp r11d, esi
  jg >
  cmp ebx, r9d
  je .done
  add ebp, esi
  add ebx, edx
  dec r13d
: cmp r13d, 0
  jl .done
  jmp <<<

.done:
  mov eax, r14d
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  ret

; from x: eax, from y: ebx
; -> eax
char_visible:
  push r8
  push r9
  push r10

  mov r8d, eax
  mov r9d, ebx

  lea rsi, [entities]
  movzx ecx, m8 [rsi+4+0] ; char x
  movzx edx, m8 [rsi+4+1] ; char y

  ; abs(x1 - x0)
  sub ecx, eax
  mov esi, ecx
  sar esi, 31
  add ecx, esi
  xor ecx, esi

  ; abs(y1 - y0)
  sub edx, ebx
  mov edi, edx
  sar edi, 31
  add edx, edi
  xor edx, edi

  xor eax, eax
  add ecx, edx
  cmp ecx, 5
  jg .done

  mov r10d, -5
: mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, r10d
  add edx, 5
  mov esi, 5
  call bresenham
  test eax, eax
  jnz .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, r10d
  sub edx, 5
  mov esi, 5
  call bresenham
  test eax, eax
  jnz .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, 5
  add edx, r10d
  mov esi, 5
  call bresenham
  test eax, eax
  jnz .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  sub ecx, 5
  add edx, r10d
  mov esi, 5
  call bresenham
  test eax, eax
  jnz .done

  inc r10d
  cmp r10d, 5
  jle <

  xor eax, eax

.done:
  pop r10
  pop r9
  pop r8
  ret

; x: ebx, y: ecx, color: edx
draw_point:
  lea rax, [screen]
  imul ebx, ebx, 24 * 4
  imul ecx, ecx, 24 * 4 * 480
  ; add ebx, 8 * 4
  ; add ecx, 8 * 4 * 480
  add rax, rbx
  add rax, rcx
  mov m32 [rax], edx
  mov m32 [rax+4], edx
  mov m32 [rax+8], edx
  mov m32 [rax+12], edx
  mov m32 [rax+480*4], edx
  mov m32 [rax+480*4+4], edx
  mov m32 [rax+480*4+8], edx
  mov m32 [rax+480*4+12], edx
  mov m32 [rax+480*2*4], edx
  mov m32 [rax+480*2*4+4], edx
  mov m32 [rax+480*2*4+8], edx
  mov m32 [rax+480*2*4+12], edx
  mov m32 [rax+480*3*4], edx
  mov m32 [rax+480*3*4+4], edx
  mov m32 [rax+480*3*4+8], edx
  mov m32 [rax+480*3*4+12], edx
  ret

clear_map:
  lea rax, [tilemap]
  xor ebx, ebx
: mov m32 [rax+rbx*4], 0
  inc ebx
  cmp ebx, 75
  jl <
  ret

generate_map:
  push r8
  push r9
  push r10
  sub rsp, 32 ; max rooms * 4

  xor r8d, r8d ; room count

  ; place first room
  mov rbx, rsp
  call random_room
  inc r8d

  mov r9d, 500 ; max number of attempts
.next_room:
  lea rbx, [rsp+r8*4]
  call random_room
  lea rbx, [rsp+r8*4]
  lea rcx, [rsp]
  mov edx, r8d
  call room_placeable
  test eax, eax
  jnz .next_attempt
  inc r8d
  cmp r8d, 8
  je .rooms_generated
.next_attempt:
  dec r9d
  jnz .next_room

.rooms_generated:
  ; place rooms
  xor r9d, r9d
.place_next:
  movzx eax, m8 [rsp+r9*4+0]
  movzx ebx, m8 [rsp+r9*4+1]
  movzx ecx, m8 [rsp+r9*4+2]
  movzx edx, m8 [rsp+r9*4+3]
  call place_room
  inc r9d
  cmp r9d, r8d
  jl .place_next

  ; connect rooms
  mov r9, rsp
.next_connection:
  cmp r8d, 2
  jl .rooms_connected
  ; get center coords for first room
  movzx eax, m8 [r9+0]
  movzx ebx, m8 [r9+1]
  movzx ecx, m8 [r9+2]
  movzx edx, m8 [r9+3]
  sub ecx, eax
  sub edx, ebx
  shr ecx, 1
  shr edx, 1
  add eax, ecx
  add ebx, edx
  ; get center coords for second room
  movzx ecx, m8 [r9+4]
  movzx edx, m8 [r9+5]
  movzx esi, m8 [r9+6]
  movzx edi, m8 [r9+7]
  sub esi, ecx
  sub edi, edx
  shr esi, 1
  shr edi, 1
  add ecx, esi
  add edx, edi
  call place_connection
  add r9, 4
  dec r8d
  jmp .next_connection
.rooms_connected:

  call place_entities

  ; postprocess
  mov r8d, 20
  lea r9, [tilemap]
  add r9, 20
.next_tile:
  lea rax, [r9+r8]
  cmp m8 [rax], 0
  jne .no_change
  sub rax, 20
  test m8 [rax], 0x80
  jz .no_change
  mov m8 [r9+r8], 2
.no_change:
  inc r8d
  cmp r8d, 280
  jl .next_tile

  ; randomly place walls
  mov r10d, 1000
  mov r9d, 15
  mov eax, 18
: test r10d, r10d
  jz .done
  call random_int
  inc eax
  mov r8d, eax
  mov eax, 12
  call random_int
  inc eax
  imul eax, eax, 20
  add eax, r8d
  lea rbx, [tilemap]
  add rbx, rax
  dec r10d
  test m8 [rbx], 0x3
  jnz <
  cmp m8 [rbx+20], 0x81
  jne <
  mov m8 [rbx], 3
  dec r9d
  jnz <

.done:
  add rsp, 32
  pop r10
  pop r9
  pop r8
  ret

place_entities:
  push r8
  push r9
  push r10

  lea r8, [entities+4]
  mov r9d, 1

  ; generate random coords
: mov eax, 18
  call random_int
  inc eax
  mov r10d, eax
  mov eax, 13
  call random_int
  inc eax

  lea rbx, [tilemap]
  imul ecx, eax, 20
  add ecx, r10d
  ; check if tile is walkable
  test m8 [rbx+rcx], 0x80
  jz <
  ; check if tile already has an entity
  test m8 [rbx+rcx], 0x70
  jnz <

  ; add entity id to tile
  mov edx, r9d
  shl edx, 4
  or m8 [rbx+rcx], dl

  mov [r8+0], r10b
  mov [r8+1], al
  add r8, 4
  inc r9d
  cmp r9d, 5
  jl <

  pop r10
  pop r9
  pop r8
  ret

; room dst: rbx
random_room:
  push r8
  push r9
  push r10
  push r11
  push r12
  ; (r12) store room dst
  mov r12, rbx
  ; (r10d) randomly pick width
  mov eax, 2
  call random_int
  add eax, 1
  mov r10d, eax
  ; (r11d) randomly pick height
  mov eax, 2
  call random_int
  add eax, 1
  mov r11d, eax
  ; (r8d) randomly pick x0
  mov eax, 18
  sub eax, r10d
  call random_int
  inc eax
  mov r8d, eax
  ; (r9d) randomly pick y0
  mov eax, 13
  sub eax, r11d
  call random_int
  inc eax
  mov r9d, eax
  ; (r10d, r11d) compute x1, y1
  add r10d, r8d
  add r11d, r9d
  ; if x0 > x1, swap them
  cmp r8d, r10d
  jle >
  xchg r8d, r10d
  ; if y0 > y1, swap them
: cmp r9d, r11d
  jle >
  xchg r9d, r11d
  ; store the room in the rooms array
: mov [r12+0], r8b
  mov [r12+1], r9b
  mov [r12+2], r10b
  mov [r12+3], r11b
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  ret

; room: rbx, rooms: rcx, count: edx
room_placeable:
  push r8
  push r9
  push r10
  push r11
  mov r8, rbx   ; room
  mov r9, rcx   ; rooms
  mov r10d, edx ; count
  mov r11, 1
: mov rbx, r8
  mov rcx, r9
  call room_intersects
  test eax, eax
  jnz >
  add r9, 4
  dec r10d
  jnz <
  xor r11d, r11d
: mov eax, r11d
  pop r11
  pop r10
  pop r9
  pop r8
  ret

; new room: rbx, existing room: rcx
; -> eax
room_intersects:
  xor eax, eax
  ; ax0 > bx1?
  movzx edx, m8 [rbx]
  movzx edi, m8 [rcx+2]
  inc edi
  cmp edx, edi
  jg >
  ; ax1 < bx0?
  movzx edx, m8 [rbx+2]
  movzx edi, m8 [rcx]
  dec edi
  cmp edx, edi
  jl >
  ; ay0 > by1?
  movzx edx, m8 [rbx+1]
  movzx edi, m8 [rcx+3]
  inc edi
  cmp edx, edi
  jg >
  ; ay1 < by0?
  movzx edx, m8 [rbx+3]
  movzx edi, m8 [rcx+1]
  dec edi
  cmp edx, edi
  jl >
  mov eax, 1
: ret

; x0: eax, y0: ebx, x1: ecx, y1: edx
place_room:
  ; if x0 > x1, swap them
  cmp eax, ecx
  jle >
  xchg eax, ecx
  ; if y0 > y1, swap them
: cmp ebx, edx
  jle >
  xchg ebx, edx
: lea rsi, [tilemap]
  imul edi, ebx, 20
  add rsi, rdi
: mov edi, eax
: mov m8 [rsi+rdi], 0x81
  inc edi
  cmp edi, ecx
  jle <
  add rsi, 20
  inc ebx
  cmp ebx, edx
  jle <<
  ret

; x0: eax, y0: ebx, x1: ecx, y1: edx
place_connection:
  push r8
  push r9
  push r10
  push r11
  mov r8d, eax ; store x0
  mov r9d, ebx ; store y0
  mov r10d, ecx ; store x1
  mov r11d, edx ; store y1
  call xorshift32
  and eax, 1
  jz .vert_first
  mov eax, r8d
  mov ebx, r10d
  mov ecx, r9d
  call place_connection_horz
  mov eax, r9d
  mov ebx, r11d
  mov ecx, r10d
  call place_connection_vert
  jmp .done
.vert_first:
  mov eax, r9d
  mov ebx, r11d
  mov ecx, r8d
  call place_connection_vert
  mov eax, r8d
  mov ebx, r10d
  mov ecx, r11d
  call place_connection_horz
.done:
  pop r11
  pop r10
  pop r9
  pop r8
  ret

; x0: eax, x1: ebx, y: ecx
place_connection_horz:
  ; (rsi) compute initial offset into the tilemap
  lea rsi, [tilemap]
  imul ecx, ecx, 20
  add rsi, rcx
  ; (edx) compute loop delta
  mov edx, 1
  mov edi, -1
  cmp eax, ebx
  cmovg edx, edi
  add ebx, edx
: mov m8 [rsi+rax], 0x81
  add eax, edx
  cmp eax, ebx
  jne <
  ret

; y0: eax, y1: ebx, x: ecx
place_connection_vert:
  ; compute p1, p2
  lea rsi, [tilemap]
  add rsi, rcx
  imul rax, rax, 20
  imul rbx, rbx, 20
  add rax, rsi
  add rbx, rsi
  ; if p1 > p2, swap them
  cmp rax, rbx
  jle >
  xchg rax, rbx
: mov m8 [rax], 0x81
  add rax, 20
  cmp rax, rbx
  jle <
  ret

; -> rng state: eax
xorshift32:
  mov eax, [rng_state]
  ; x ^= x << 13
  mov ebx, eax
  shl ebx, 13
  xor eax, ebx
  ; x ^= x >> 17
  mov ebx, eax
  shr ebx, 17
  xor eax, ebx
  ; x ^= x << 5
  mov ebx, eax
  shl ebx, 5
  xor eax, ebx
  mov [rng_state], eax
  ret

; max: eax
; -> result: eax
random_int:
  push r8
  mov r8d, eax
  call xorshift32
  xor edx, edx
  div r8d
  mov eax, edx
  pop r8
  ret

xauth: .i8 "XAUTHORITY="

sockaddr:
  .i8 1 0 "/tmp/.X11-unix/X0"
  .res 91

conn_request:
  .i8 108 0                    ; ASCII 'l' to specify little endian
  .i16 11                      ; major version
  .i16 0                       ; minor version
  .i16 18                      ; auth protocol name length
  .i16 16                      ; auth protocol data length
  .i8 0 0                      ; pad
  .i8 "MIT-MAGIC-COOKIE-1" 0 0 ; auth protocol name
.cookie: .res 16

create_window:
  .i8 1            ; opcode
  .i8 0            ; depth
  .i16 9           ; length
.id:
  .i32 0           ; window id
.parent:
  .i32 0           ; parent id
  .i16 0 0 480 360 ; x, y, width, height
  .i16 0 1         ; border, class
  .i32 0           ; visual
  .i32 0x00000800  ; event-mask
  .i32 0x00008001  ; keypress + exposure

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
.window:
  .i32 0                     ; window id
  .i32 67 31                 ; property (WM_CLASS), type (STRING)
  .i32 8                     ; format
  .i32  11                   ; length
  .i8 "exile" 0 "exile" 0

change_wm_name:
  .i8 18 0 ; opcode, "replace"
  .i16 8 ; length
.window:
  .i32 0 ; window id
  .i32 39 31 ; property (WM_NAME), type (STRING)
  .i32 8 ; format
  .i32 5 ; length
  .i8 "exile" 0 0 0

change_wm_protocols:
  .i8 18 0 ; opcode, "replace"
  .i16 7 ; length
.window:
  .i32 0 ; window id
.property:
  .i32 0 
  .i32 4 ; type (ATOM)
  .i32 32 ; format
  .i32 1 ; length
.value:
  .i32 0

intern_atom_wm_protocols:
  .i8 16 0 5 0
  .i32 12; length of name
  .i8 "WM_PROTOCOLS"

intern_atom_wm_delete_window:
  .i8 16 0 6 0
  .i32 16; length of name
  .i8 "WM_DELETE_WINDOW"

tiles:
  .i16 0x2000 0x1048 0x0002 0x9480 0x0100 0x4000 0x4020 0x9100 0x2004
  .i16 0xf000 0xf77f 0x77e7 0x01d9 0x0000 0x0000 0x0000 0x0000 0x0000
  .i16 0xf000 0xd97f 0x7fb6 0xd6df 0xff4e 0x0007 0xd7ff 0xba55 0x5552
  .i16 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0004 0x0000

sprites:
  .i32 0x8002aa00 0x56a00aaa 0x2955a02a 0xa82996a0 0x5aa829a6 0x2aa9a82a 0x5a29a56a 0xa96aa5a5 0x2aaaa8a6
  .i32 0x00000000 0xaa800000 0x0aaaa800 0x6a2a95aa 0x5a6a2a55 0x2a556a2a 0x9a29999a 0xa6a829a6 0x02aaa00a

rng_state: .i32 0
sockfd: .i32 0

; each entity has x, y, sprite and 'sees char' flag
entities:
  .i8 0 0 0 0; invalid entity
  .i8 0 0 0 0
  .i8 0 0 1 0
  .i8 0 0 1 0
  .i8 0 0 1 0

; tile bits: WEEER-SS
; W = walkable
; E = entity id
; R = needs to be redrawn
; S = sprite
tilemap:
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

put_image:
  .i8 72             ; opcode
  .i8 2              ; format (ZPixmap)
  .i16 0             ; length (0 to allow big request)
  .i32 7 + 480 * 360 ; actual length
.window:
  .i32 0             ; window id
.gc:
  .i32 0             ; gc id
  .i16 480 360 0 0   ; width, height,x, y
  .i8 0 24 0 0       ; left-pad, depth
screen:
  .res 480 * 360 * 4
