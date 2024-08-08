.def map_width  24
.def map_height 14
.def tile_size  24

.def screen_width  map_width * tile_size
.def screen_height (map_height + 1) * tile_size
.def screen_pitch  screen_width*4

.def tm_entity_shift 3
.def tm_walkable 0b10000000
.def tm_opaque   0b01000000
.def tm_entity   0b111 << tm_entity_shift
.def tm_redraw   0b00000100
.def tm_sprite   0b00000011

.def e_xpos  0
.def e_ypos  1
.def e_data  2
.def e_flags 3

.def ef_triggered 1 << 0
.def ef_hurt      1 << 1
.def ef_stunned   1 << 2

.def em_hp   0b00001111
.def em_type 0b11110000

.def tile_ground 1
.def tile_cliff  2
.def tile_wall   3

.def entity_char    1 << 4
.def entity_crawler 2 << 4

.def char_hp    5
.def crawler_hp 2

.def ab_dash 1

.def max_entities 8
.def max_enemies  3 ; must be <= max_entities - 2

.def max_rooms    7 ; must be >= 2
.def max_walls    20
.def room_min_dim 2
.def room_max_dim 3

.def fov_distance 6

.def ui_x 3
.def ui_y screen_height/2 - 5 - 3

.macro sys_write fd addr size {
  mov eax, 1
  mov edi, @fd
  lea rsi, @addr
  mov edx, @size
  syscall
}

.macro sys_read fd dst size {
  xor eax, eax
  mov edi, @fd
  mov rsi, @dst
  mov edx, @size
  syscall
}

.macro write_pixel dst rgb {
  mov m32 [@dst], @rgb
  mov m32 [@dst+4], @rgb
  mov m32 [@dst+screen_pitch], @rgb
  mov m32 [@dst+screen_pitch+4], @rgb
}

.macro cxchg.le a b {
  cmp @a, @b
  jle >
  xchg @a, @b
:
}

.macro cjmp.e a b dst {
  cmp @a, @b
  je @dst
}

.macro cjmp.ne a b dst {
  cmp @a, @b
  jne @dst
}

.macro cjmp.g a b dst {
  cmp @a, @b
  jg @dst
}

.macro cjmp.l a b dst {
  cmp @a, @b
  jl @dst
}

.macro cjmp.ae a b dst {
  cmp @a, @b
  jae @dst
}

.macro cjmp.z r dst {
  test @r, @r
  jz @dst
}

.macro cjmp.nz r dst {
  test @r, @r
  jnz @dst
}

.macro tjmp.z a b dst {
  test @a, @b
  jz @dst
}

.macro tjmp.nz a b dst {
  test @a, @b
  jnz @dst
}

.macro min a b {
  cmp @a, @b
  cmovl @a, @b
}

.macro max a b {
  cmp @a, @b
  cmovg @a, @b
}

.macro clamp x low high {
  cmp @x, @low
  cmovl @x, @low
  cmp @x, @high
  cmovg @x, @high
}

; TODO: this could use a better name
.macro dcjmp.nz reg dst {
  dec @reg
  jnz @dst
}

; TODO: this could use a better name
.macro icjmp.l reg limit dst {
  inc @reg
  cmp @reg, @limit
  jl @dst
}

.macro screen_offset dst x y {
  lea @dst, [screen]
  shl @x, 3
  imul @y, @y, screen_pitch * 2
  add @dst, @x
  add @dst, @y
}

.macro cursor_rect x y w h {
  mov eax, @x - 2
  mov ebx, @y - 2
  mov ecx, @w + 2 - 3
  mov edx, @h + 2 - 2
}

read_xauthority_cookie:
  mov rsi, [rsp]          ; get number of command line args
  lea rsi, [rsp+rsi*8+16] ; get address of first environment var
  lea rbx, [xauth]        ; "XAUTHORITY" string
: mov rcx, [rsi]          ; tested env variable
  ; zero means end of env var array
  ; if there is no $XAUTHORITY go straight to opening a connection
  cjmp.z ecx, open_connection
  add rsi, 8
  xor edx, edx               ; loop counter
: mov al, [rcx+rdx]          ; read character from tested var
  cjmp.ne al, [rbx+rdx], <<  ; compare it to the xauth string
  inc edx
  cjmp.ne edx, 11, <         ; reached end of xauth string?

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

; connect to X11
sys_write r12d, [conn_request], 48
sub rsp, 8276 ; TODO: should get this from the response
sys_read r12d, rsp, 8276

cjmp.e m8 [rsp], 0, exit_error ; zero first byte of the reply indicates connection failure

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

; enable big requests extension
sys_write r12d, [query_big_requests], 20
sys_read r12d, rsp, 32
mov al, [rsp+9]
mov [big_req_enable], al
sys_write r12d, [big_req_enable], 4
sys_read r12d, rsp, 32 ; request always generates a reply

sys_write r12d, [create_window], 36
sys_write r12d, [change_wm_class], 36
sys_write r12d, [change_wm_name], 32

; intern WM_PROTOCOLS and WM_DELETE_WINDOW atoms
sys_write r12d, [intern_atom_wm_protocols], 20
sys_write r12d, [intern_atom_wm_delete_window], 24
sys_read r12d, rsp, 64
movzx eax, m16 [rsp+8]
mov [change_wm_protocols.property], eax
movzx ebx, m16 [rsp+32+8]
mov [change_wm_protocols.value], ebx

sys_write r12d, [change_wm_protocols], 28
sys_write r12d, [create_gc], 16
sys_write r12d, [map_window], 8

add rsp, 8276
mov [sockfd], r12d

; seed rng
rdrand eax
mov [rng_state], eax

call generate_map
call draw_world

sub rsp, 32

main_loop:
  ; store current time
  mov eax, 228 ; clock_gettime
  mov edi, 1 ; CLOCK_MONOTONIC
  lea rsi, [rsp]
  syscall
  mov rax, [rsp+0]
  mov rbx, [rsp+8]
  imul rax, rax, 1000000000
  add rax, rbx
  mov [time], rax

  ; check if the timer was active and if it ran out
  cjmp.e m64 [timer], 0, >>>
  cjmp.l rax, [timer], >>>

  ; update entities
  lea rsi, [entities]
  mov edx, 1
: movzx ebx, m8 [rsi+rdx*4+e_data]
  tjmp.z ebx, em_type, >

  and m8 [rsi+rdx*4+e_flags], ~(ef_hurt | ef_stunned)

  and ebx, em_hp
  cjmp.g ebx, 0, >
  ; entity is out of hp
  mov m8 [rsi+rdx*4+e_data], 0

  ; remove entity id from tile
  movzx eax, m8 [rsi+rdx*4+e_xpos]
  movzx ebx, m8 [rsi+rdx*4+e_ypos]
  imul ebx, ebx, map_width
  add eax, ebx
  lea rcx, [tilemap]
  and m8 [rcx+rax], ~tm_entity

: icjmp.l edx, max_entities, <<

  mov m64 [timer], 0

  call redraw_tilemap
  jmp refresh_screen

: mov ebx, [sockfd]

  ; build pollfd structure
  mov edi, 1
  shl rdi, 32
  or rdi, rbx
  mov [rsp], rdi
  mov m32 [rsp+0], ebx
  mov m32 [rsp+4], 1

  ; poll the socket with a timeout
  mov eax, 7
  lea rdi, [rsp]
  mov esi, 1
  mov rdx, 5
  syscall
  cjmp.e eax, 0, main_loop

; read event
  sys_read ebx, rsp, 32
  movzx eax, m8 [rsp] ; load event id
  cjmp.e eax, 2, process_keydown ; keypress event?
  cjmp.e eax, 12, refresh_screen ; expose event?
  cjmp.ne eax, 161, main_loop ; what is this event?
  mov eax, [rsp+8]
  mov ebx, [change_wm_protocols.property]
  cjmp.ne eax, ebx, main_loop
  mov eax, [rsp+12]
  mov ebx, [change_wm_protocols.value]
  cjmp.ne eax, ebx, main_loop
  jmp exit

refresh_screen:
  sys_write [sockfd], [put_image], (7 + screen_width * screen_height) * 4
  jmp main_loop

process_keydown:
  movzx eax, m8 [rsp+1] ; read keycode
  cjmp.e eax, 0x18, exit        ; is it 'Q'
  cjmp.e eax, 0x72, .move_right ; is it right arrow key
  cjmp.e eax, 0x2e, .move_right ; is it 'l' key
  cjmp.e eax, 0x74, .move_down  ; is it down arrow key
  cjmp.e eax, 0x2c, .move_down  ; is it 'j' key
  cjmp.e eax, 0x71, .move_left  ; is it left arrow key
  cjmp.e eax, 0x2b, .move_left  ; is it 'h' key
  cjmp.e eax, 0x6f, .move_up    ; is it up arrow key
  cjmp.e eax, 0x2d, .move_up    ; is it 'k' key

  cjmp.ne eax, 0x42, >          ; is it escape key
  mov m8 [selected_ability], 0
  call draw_ui
  jmp refresh_screen

: cjmp.ne eax, 0x26, >          ; is it 'a' key
  mov m8 [selected_ability], ab_dash
  call draw_ui
  jmp refresh_screen

: cjmp.ne eax, 0x60, > ; is it 'f12' key
  not m8 [debugmode]
  call clear_screen
  call draw_world
  jmp refresh_screen

: cjmp.ne eax, 0x1b, main_loop ; is it 'r' key
  call clear_map
  call generate_map
  call clear_screen
  call draw_world
  jmp refresh_screen
.move_right:
  mov ebx, 1
  xor ecx, ecx
  call move_char
  cjmp.z eax, main_loop
  jmp refresh_screen
.move_down:
  xor ebx, ebx
  mov ecx, 1
  call move_char
  cjmp.z eax, main_loop
  jmp refresh_screen
.move_left:
  mov ebx, -1
  xor ecx, ecx
  call move_char
  cjmp.z eax, main_loop
  jmp refresh_screen
.move_up:
  xor ebx, ebx
  mov ecx, -1
  call move_char
  cjmp.z eax, main_loop
  jmp refresh_screen

exit_error:
  mov eax, 60
  mov edi, 1
  syscall

exit:
  mov eax, 60
  xor edi, edi
  syscall

; num: eax, x: ebx, y: ecx, color: edx
draw_number:
  .push r8 r9 r10
  sub rsp, 8

  mov r8d, edx

  ; store individual digits on the stack
  mov esi, 10
  xor r9d, r9d
: xor edx, edx
  div esi
  mov [rsp+r9], dl
  inc r9d
  cjmp.nz eax, <

  ; lea r10, [screen]
  ; imul ebx, ebx, 4
  ; imul ecx, ecx, screen_pitch
  ; add r10, rbx
  ; add r10, rcx
  screen_offset r10, rbx, rcx

: movzx edx, m8 [rsp+r9-1]
  lea rdi, [font]
  movzx edx, m16 [rdi+rdx*2+2]
  mov rdi, r10
  xor ebx, ebx ; y counter
: xor ecx, ecx ; x counter
: tjmp.z dl, 1, >
  write_pixel rdi+rcx*8, r8d
: shr edx, 1
  icjmp.l ecx, 3, <<
  add rdi, screen_pitch*2
  icjmp.l ebx, 5, <<<
  add r10, 4 * 8
  dcjmp.nz r9d, <<<<

  add rsp, 8
  .pop
  ret

; x: eax, y: ebx, rgb: ecx, str: rdx, len: esi
draw_text:
  .push r8 r9

  screen_offset rdi, rax, rbx
  mov r9, rdi
  mov ebp, ecx

  xor r8d, r8d ; char counter
: movzx eax, m8 [rdx+r8]
  sub eax, 33
  cmp eax, 93
  ja .next
  lea rbx, [glyphmap]
  lea rcx, [font]
  movzx eax, m8 [rbx+rax]
  movzx eax, m16 [rcx+rax*2]

  mov rdi, r9
  xor ebx, ebx ; y counter
: xor ecx, ecx ; x counter
: tjmp.z al, 1, >
  write_pixel rdi+rcx*8, ebp
: shr eax, 1
  icjmp.l ecx, 3, <<
  add rdi, screen_pitch*2
  icjmp.l ebx, 5, <<<
.next:
  add r9, 8*4
  icjmp.l r8d, esi, <<<<

  .pop
  ret

draw_distbuf:
  .push r8 r9 r10
  lea r10, [distbuf]
  xor r9d, r9d ; y counter
: xor r8d, r8d ; x counter
: movzx eax, m8 [r10]
  cjmp.z eax, >
  cmp eax, 0xff
  jge >
  mov ebx, r8d
  mov ecx, r9d
  imul ebx, ebx, tile_size
  imul ecx, ecx, tile_size
  mov edx, 0xff00ab
  call draw_number
: inc r10
  icjmp.l r8d, map_width, <<
  icjmp.l r9d, map_height, <<<
  .pop
  ret

; x: eax, y: ebx, sprite: ecx, color: edx
draw_sprite:
  .push r8 r9 r10

  mov r10d, edx

  lea rdx, [screen]
  imul eax, eax, tile_size * 4
  imul ebx, ebx, tile_size * screen_width * 4
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
  and ebp, 0b11
  jz >
  mov esi, r10d
  mov edi, 0x000000
  cmp ebp, 0x2
  cmove esi, edi
  write_pixel rdx, esi
: inc r9d
  cjmp.ne r9d, 12, >
  add rdx, (screen_width * 2 - tile_size) * 4
  xor r9d, r9d
: add rdx, 8
  shr eax, 2
  dcjmp.nz ebx, <<<
  icjmp.l r8d, 9, <<<<

  .pop
  ret

; x: eax, y: ebx, sprite: ecx
draw_sprite_negative:
  .push r8 r9 r10

  mov r10d, edx

  lea rdx, [screen]
  imul eax, eax, tile_size * 4
  imul ebx, ebx, tile_size * screen_width * 4
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
  and ebp, 0b11
  mov esi, 0xffffff
  mov edi, 0x000000
  cmp ebp, 0x1
  cmove esi, edi
  write_pixel rdx, esi
: inc r9d 
  cjmp.ne r9d, 12, >
  add rdx, (screen_width * 2 - tile_size) * 4
  xor r9d, r9d
: add rdx, 8
  shr eax, 2
  dcjmp.nz ebx, <<<
  icjmp.l r8d, 9, <<<<

  .pop
  ret

; entity id: eax
draw_entity:
  .push r10
  mov r10d, eax
  lea rax, [entities]
  lea r10, [rax+r10*4]
  movzx eax, m8 [r10+e_xpos]
  movzx ebx, m8 [r10+e_ypos]
  movzx ecx, m8 [r10+e_data]
  shr ecx, 4
  cjmp.z ecx, .done
  dec ecx
  tjmp.z m8 [r10+e_flags], ef_hurt, >
  call draw_sprite_negative
  jmp >>
: mov edx, 0xffffff
  call draw_sprite

: cjmp.e m8 [debugmode], 0, .done
  ; draw triggered indicator
  tjmp.z m8 [r10+e_flags], ef_triggered, >
  movzx ebx, m8 [r10+e_xpos]
  movzx ecx, m8 [r10+e_ypos]
  mov edx, 0xff0000
  call draw_point
  ; draw hp
: movzx eax, m8 [r10+e_data]
  and eax, em_hp
  movzx ebx, m8 [r10+e_xpos]
  imul ebx, ebx, tile_size
  movzx ecx, m8 [r10+e_ypos]
  imul ecx, ecx, tile_size
  mov edx, 0xab00bb
  call draw_number

.done:
  .pop
  ret

; sprite: eax, x: ebx, y: ecx
draw_tile:
  .push r8
  lea rsi, [tiles-9*2]
  lea rdi, [screen]
  imul ebx, ebx, 24 * 4
  imul ecx, ecx, 24 * 4 * screen_width
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
  write_pixel rdi, r8d
  add rdi, 8
  dcjmp.nz edx, >
  add rdi, screen_pitch*2 - tile_size*4
  mov edx, 12
: shr eax, 1
  dcjmp.nz ebx, <<
  add rsi, 2
  dcjmp.nz ecx, <<<
  .pop
  ret

; x0: eax, y0: ebx, x1: ecx, y1: edx, color: esi
draw_rect:
  lea rdi, [screen]
  imul ebp, ebx, screen_pitch
  add rdi, rbp
  mov ebp, eax
: mov eax, ebp
: mov [rdi+rax*4], esi
  icjmp.l eax, ecx, <
  add rdi, screen_pitch
  icjmp.l ebx, edx, <<
  ret

draw_tilemap:
  .push r8 r9 r10
  lea r8, [tilemap]
  xor r10d, r10d ; y counter
: xor r9d, r9d ; x counter
: movzx eax, m8 [r8]
  inc r8
  and eax, tm_sprite
  jz >
  mov ebx, r9d
  mov ecx, r10d
  call draw_tile
: icjmp.l r9d, map_width, <<
  icjmp.l r10d, map_height, <<<
  .pop
  ret

redraw_tilemap:
  .push r8 r9 r10 r11
  lea r8, [tilemap]
  xor r10d, r10d ; y counter
: xor r9d, r9d ; x counter
: movzx r11d, m8 [r8]
  ;test r11b, tm_redraw
  tjmp.z r11b, tm_walkable, >
  mov eax, r11d
  and eax, tm_sprite
  mov ebx, r9d
  mov ecx, r10d
  call draw_tile
  tjmp.z r11b, tm_entity, >
  mov eax, r11d
  and eax, tm_entity
  shr eax, tm_entity_shift
  call draw_entity
  and m8 [r8], ~tm_redraw
: inc r8
  inc r9d
  cjmp.ne r9d, map_width, <<
  inc r10d
  cjmp.ne r10d, map_height, <<<
  .pop
  ret

draw_world:
  .push r8 r9 r10
  call draw_tilemap

  lea r8, [entities]
  mov r9d, 1
: mov eax, r9d
  cjmp.e m8 [r8+r9*4+2], 0, >
  call draw_entity
: icjmp.l r9d, max_entities, <<

  cjmp.e m8 [debugmode], 0, >
  mov eax, [map_seed]
  mov ebx, 4
  mov ecx, 4
  mov edx, 0xafa08f
  call draw_number

: call draw_ui

: .pop
  ret

clear_screen:
  ; TODO: rep stosb?
  lea rax, [screen]
  xor ebx, ebx
: mov m64 [rax+rbx*8], 0
  icjmp.l ebx, (screen_width * screen_height * 4)/8, <
  ret

draw_ui:
  ; clear part of screen that is covered by ui
  lea rax, [screen+map_height*tile_size*screen_pitch]
  xor ebx, ebx
: mov m64 [rax+rbx*8], 0
  icjmp.l ebx, (screen_pitch*tile_size)/8, <

  mov eax, ui_x
  mov ebx, ui_y
  mov ecx, 0xffffff
  lea rdx, [str_hp]
  mov esi, 2
  call draw_text

  movzx eax, m8 [entities+4+e_data]
  and eax, em_hp
  mov ebx, ui_x + 12
  mov ecx, ui_y
  mov edx, 0xffffff
  call draw_number

  mov eax, ui_x + 12 + 4*8
  mov ebx, ui_y
  mov ecx, 0xffffff
  lea rdx, [str_dash]
  mov esi, 4
  call draw_text

  cjmp.ne m8 [selected_ability], 1, >
  cursor_rect ui_x + 12 + 4*8, ui_y, 4*4, 5
  call draw_cursor

: ret

; x: eax, y: ebx, w: ecx, h: edx
draw_cursor:
  .push r8

  mov r8d, eax
  screen_offset rsi, rax, rbx
  mov rbp, m64 [cursor]
  mov rdi, rsi
  shl ecx, 3
  imul rdx, rdx, screen_pitch*2

  ; top-left corner
  xor ebx, ebx ; y counter
: xor eax, eax ; x counter
: tjmp.z ebp, 1, >
  write_pixel rdi+rax*8, 0xffffff
: shr rbp, 1
  icjmp.l eax, 4, <<
  add rdi, screen_pitch*2
  icjmp.l ebx, 4, <<<

  ; top-right corner
  mov rdi, rsi
  add rdi, rcx
  xor ebx, ebx ; y counter
: xor eax, eax ; x counter
: tjmp.z ebp, 1, >
  write_pixel rdi+rax*8, 0xffffff
: shr rbp, 1
  icjmp.l eax, 4, <<
  add rdi, screen_pitch*2
  icjmp.l ebx, 4, <<<

  ; bottom-left corner
  add rsi, rdx
  mov rdi, rsi
  xor ebx, ebx ; y counter
: xor eax, eax ; x counter
: tjmp.z ebp, 1, >
  write_pixel rdi+rax*8, 0xffffff
: shr rbp, 1
  icjmp.l eax, 4, <<
  add rdi, screen_pitch*2
  icjmp.l ebx, 4, <<<

  ; bottom-right corner
  add rsi, rcx
  xor ebx, ebx ; y counter
: xor eax, eax ; x counter
: tjmp.z ebp, 1, >
  write_pixel rsi+rax*8, 0xffffff
: shr rbp, 1
  icjmp.l eax, 4, <<
  add rsi, screen_pitch*2
  icjmp.l ebx, 4, <<<

  .pop
  ret

; @ebx: delta x
; @ecx: delta y
; -> if moved: eax
move_char:
  sub rsp, 16

  mov m8 [do_timer], 0

  cjmp.ne m64 [timer], 0, >>>>               ; prevent action when timer is active
  tjmp.z m8 [entities+4+e_data], em_hp, >>>> ; prevent action when char has 0 hp

  cjmp.ne m8 [selected_ability], ab_dash, >
  call use_dash
  jmp >>

: mov edx, 1
  call move_entity

: cjmp.z eax, >>
  cjmp.e eax, 0xff, >
  call damage_entity

: call simulate_entities
  call redraw_tilemap
  call draw_ui

  ; start the timer if necessary
  cjmp.ne m8 [do_timer], 1, >
  mov rax, [time]
  add rax, 150 * 1000000
  mov [timer], rax

  ;call draw_distbuf
: mov eax, 1
: add rsp, 16
  ret

; @ebx: delta x
; @ecx: delta y
; -> if used: eax
use_dash:
  .push r8
  xor eax, eax

  movzx esi, m8 [char+e_xpos]
  movzx edi, m8 [char+e_ypos]
  lea rdx, [tilemap]

  ; store original char tilemap offset
  imul r8d, edi, map_width
  add r8d, esi

  ; compute new char tilemap offset
  add esi, ebx
  add edi, ecx
  imul rbp, rdi, map_width
  add rbp, rsi

  tjmp.z m8 [rdx+rbp], tm_walkable, >>
  tjmp.nz m8 [rdx+rbp], tm_entity, >>

  ; move char onto the new tile
  mov [char+e_xpos], sil
  mov [char+e_ypos], dil
  and m8 [rdx+r8], ~tm_entity
  or m8 [rdx+rbp], 1 << tm_entity_shift

  ; if the next tile contains an enemy, it will be damaged and stunned
  add esi, ebx
  add edi, ecx
  imul ebp, edi, map_width
  add ebp, esi
  movzx eax, m8 [rdx+rbp]
  and eax, tm_entity
  jz >
  lea rbx, [entities]
  shr eax, tm_entity_shift
  or m8 [rbx+rax*4+e_flags], ef_stunned
  call damage_entity

: mov m8 [selected_ability], 0
  mov eax, 0xff

: .pop
  ret

; delta x: ebx, delta y: ecx, id: edx
; -> eax, 0 if could not move, 0xff if moved, blocking entity id othwerwise
move_entity:
  .push r8 r9 r10

  xor eax, eax
  lea rsi, [tilemap]

  ; load original coords
  lea r10, [entities]
  lea r10, [r10+rdx*4]
  movzx r8d, m8 [r10+0]
  movzx r9d, m8 [r10+1]

  ; compute new coords
  add ebx, r8d
  add ecx, r9d

  ; get offset into the tilemap for the new coords
  imul edi, ecx, map_width
  add edi, ebx

  tjmp.z m8 [rsi+rdi], tm_walkable, >

  ; check if the tile is occupied by another entity
  ; TODO: this is probably only useful to char movement
  movzx eax, m8 [rsi+rdi]
  and eax, tm_entity
  shr eax, tm_entity_shift
  cjmp.nz eax, >

  ; store new entity coords
  mov [r10+0], bl
  mov [r10+1], cl

  movzx eax, m8 [rsi+rdi]
  ; add entity id to tile
  shl edx, tm_entity_shift
  or eax, edx
  or al, tm_redraw
  mov [rsi+rdi], al

  ; get offset into the tilemap for the old coords
  imul edi, r9d, map_width
  add edi, r8d
  movzx eax, m8 [rsi+rdi]
  ; remove entity id from tile
  and al, ~tm_entity
  or al, tm_redraw
  mov [rsi+rdi], al

  mov eax, 0xff
: .pop
  ret

simulate_entities:
  .push r8 r9
  mov r8d, 2
  lea r9, [entities]

.next:
  tjmp.z m8 [r9+r8*4+e_data], em_type, >>
  tjmp.nz m8 [r9+r8*4+e_flags], ef_stunned, >>

  call compute_distances

  movzx eax, m8 [r9+r8*4+e_xpos]
  movzx ebx, m8 [r9+r8*4+e_ypos]
  call char_visible
  cjmp.nz eax, >
  and m8 [r9+r8*4+e_flags], ~ef_triggered
  mov ebx, r8d
  call move_randomly
  jmp >>
: or m8 [r9+r8*4+e_flags], ef_triggered
  mov ebx, r8d
  call move_towards_char
: icjmp.l r8d, max_entities, .next
  .pop
  ret

; entity id: ebx
move_randomly:
  xor eax, eax
  mov ebx, 4
  call random_int
  cjmp.ne eax, 0, >
  mov ebx, 1
  xor ecx, ecx
  jmp .move
: cjmp.ne eax, 1, >
  mov ebx, -1
  xor ecx, ecx
  jmp .move
: cjmp.ne eax, 2, >
  xor ebx, ebx
  mov ecx, 1
  jmp .move
: xor ebx, ebx
  mov ecx, -1
  jmp .move
.move:
  mov edx, r8d
  call move_entity
  ret

; entity id: ebx
move_towards_char:
  .push r8
  mov r8d, ebx ; store entity id

  ; compute offset into tilemap for current coords
  lea rcx, [entities]
  lea rcx, [rcx+rbx*4]
  movzx eax, m8 [rcx+0]
  movzx ebx, m8 [rcx+1]
  imul ebx, ebx, map_width
  add eax, ebx

  lea rbx, [distbuf]
  lea rbp, [tilemap]
  add rbx, rax
  add rbp, rax
  mov ecx, 0xff ; best dist

  ; check adjacent tiles and pick the one with best distance
: movzx edx, m8 [rbx+1]
  cjmp.ae edx, ecx, >
  mov ecx, edx
  mov esi, 1
  mov edi, 0
: movzx edx, m8 [rbx-1]
  cjmp.ae edx, ecx, >
  mov ecx, edx
  mov esi, -1
  mov edi, 0
: movzx edx, m8 [rbx+map_width]
  cjmp.ae edx, ecx, >
  mov ecx, edx
  mov esi, 0
  mov edi, 1
: movzx edx, m8 [rbx-map_width]
  cjmp.ae edx, ecx, >
  mov ecx, edx
  mov esi, 0
  mov edi, -1
: cjmp.e ecx, 0xff, .done
  cjmp.nz ecx, >

  ; attack
  mov eax, 1
  call damage_entity
  mov m8 [do_timer], 1

  jmp .done
: mov ebx, esi
  mov ecx, edi
  mov edx, r8d
  call move_entity

.done:
  .pop
  ret

; id: eax
damage_entity:
  lea rbx, [entities]
  or m8 [rbx+rax*4+e_flags], ef_hurt
  movzx ecx, m8 [rbx+rax*4+e_data]
  mov edx, ecx
  and ecx, em_hp
  dec ecx

  ; set hp to zero if it became negative
  xor esi, esi
  cmp ecx, 0
  cmovl ecx, esi

  and edx, ~em_hp
  or ecx, edx
  mov [rbx+rax*4+e_data], cl
  mov m8 [do_timer], 1
  ret

; x0: eax, y0: ebx, x1: ecx, y1: edx, maxdist: esi
; -> hit char: eax
bresenham:
  .push r8 r9 r10 r11 r12 r13 r14

  mov r13d, esi
  xor r14d, r14d

  ; clamp coords to map bounds
  mov esi, 1
  mov edi, map_width - 2
  clamp eax, esi, edi
  clamp ecx, esi, edi
  mov edi, map_height - 2
  clamp ebx, esi, edi
  clamp edx, esi, edi

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
  imul r12d, ebx, map_width
  add r11, rax
  add r11, r12
  movzx r12d, m8 [r11]
  tjmp.nz r12b, tm_opaque, .done
  ; hit char?
  and r12b, tm_entity
  cmp r12b, 1 << tm_entity_shift
  sete r14b
  je .done
  ; compute e2
  mov r11d, ebp
  add r11d, ebp
  ; if e2 >= dy
  cjmp.l r11d, edi, >
  cjmp.e eax, r8d, .done
  add ebp, edi
  add eax, ecx
  dec r13d
  ; if e <= dx
: cjmp.g r11d, esi, >
  cjmp.e ebx, r9d, .done
  add ebp, esi
  add ebx, edx
  dec r13d
: cjmp.l r13d, 0, .done
  jmp <<<

.done:
  mov eax, r14d
  .pop
  ret

; from x: eax, from y: ebx
; -> eax
char_visible:
  .push r8 r9 r10

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
  cjmp.g ecx, fov_distance, .done

  mov r10d, -fov_distance
: mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, r10d
  add edx, fov_distance
  mov esi, fov_distance
  call bresenham
  cjmp.nz eax, .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, r10d
  sub edx, fov_distance
  mov esi, fov_distance
  call bresenham
  cjmp.nz eax, .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  add ecx, fov_distance
  add edx, r10d
  mov esi, fov_distance
  call bresenham
  cjmp.nz eax, .done

  mov eax, r8d
  mov ebx, r9d
  mov ecx, eax
  mov edx, ebx
  sub ecx, fov_distance
  add edx, r10d
  mov esi, fov_distance
  call bresenham
  cjmp.nz eax, .done

  inc r10d
  cmp r10d, fov_distance
  jle <

  xor eax, eax

.done:
  .pop
  ret

compute_distances:
  sub rsp, 64*4
  lea rcx, [distbuf]

  ; load char coords
  lea rsi, [entities]
  movzx eax, m8 [rsi+4+0]
  movzx ebx, m8 [rsi+4+1]

  ; reset distbuf
  xor edx, edx
: mov m8 [rcx+rdx], 0xff
  icjmp.l edx, map_width * map_height, <

  ; push initial coords
  imul ebx, ebx, map_width
  add eax, ebx
  mov [rsp+0], ax
  mov m8 [rsp+2], 0
  mov m8 [rcx+rax], 0

  mov edi, 1 ; stack size
  lea rsi, [tilemap]

.next:
  dec edi
  movzx eax, m16 [rsp+rdi*4] ; load offset from stack
  movzx ebx, m8 [rsp+rdi*4+2] ; load dist from stack
  inc ebx

  ; [1, 0]
  mov edx, eax
  inc edx
  tjmp.z m8 [rsi+rdx], tm_walkable, >
  tjmp.nz m8 [rsi+rdx], tm_entity, >
  cjmp.ae bl, [rcx+rdx], >
  mov [rcx+rdx], bl
  mov [rsp+rdi*4+0], dx
  mov [rsp+rdi*4+2], bl
  inc edi

  ; [-1, 0]
: mov edx, eax
  dec rdx
  tjmp.z m8 [rsi+rdx], tm_walkable, >
  tjmp.nz m8 [rsi+rdx], tm_entity, >
  cjmp.ae bl, [rcx+rdx], >
  mov [rcx+rdx], bl
  mov [rsp+rdi*4+0], dx
  mov [rsp+rdi*4+2], bl
  inc edi

  ; [0, 1]
: mov edx, eax
  add edx, map_width
  tjmp.z m8 [rsi+rdx], tm_walkable, >
  tjmp.nz m8 [rsi+rdx], tm_entity, >
  cjmp.ae bl, [rcx+rdx], >
  mov [rcx+rdx], bl
  mov [rsp+rdi*4+0], dx
  mov [rsp+rdi*4+2], bl
  inc edi

  ; [0, -1]
: mov edx, eax
  sub rdx, map_width
  tjmp.z m8 [rsi+rdx], tm_walkable, >
  tjmp.nz m8 [rsi+rdx], tm_entity, >
  cjmp.ae bl, [rcx+rdx], >
  mov [rcx+rdx], bl
  mov [rsp+rdi*4+0], dx
  mov [rsp+rdi*4+2], bl
  inc edi

: cjmp.nz edi, .next

  add rsp, 64*4
  ret

; x: ebx, y: ecx, color: edx
draw_point:
  lea rax, [screen]
  imul ebx, ebx, tile_size * 4
  imul ecx, ecx, tile_size * 4 * screen_width
  add ebx, (tile_size - 8) * 4
  add rax, rbx
  add rax, rcx
  mov m32 [rax], edx
  mov m32 [rax+4], edx
  mov m32 [rax+8], edx
  mov m32 [rax+12], edx
  mov m32 [rax+screen_pitch], edx
  mov m32 [rax+screen_pitch+4], edx
  mov m32 [rax+screen_pitch+8], edx
  mov m32 [rax+screen_pitch+12], edx
  mov m32 [rax+screen_pitch*2], edx
  mov m32 [rax+screen_pitch*2+4], edx
  mov m32 [rax+screen_pitch*2+8], edx
  mov m32 [rax+screen_pitch*2+12], edx
  mov m32 [rax+screen_pitch*3], edx
  mov m32 [rax+screen_pitch*3+4], edx
  mov m32 [rax+screen_pitch*3+8], edx
  mov m32 [rax+screen_pitch*3+12], edx
  ret

clear_map:
  lea rax, [tilemap]
  xor ebx, ebx
: mov m32 [rax+rbx*4], 0
  icjmp.l ebx, map_width * map_height / 4, <
  ret

generate_map:
  .push r8 r9 r10
  sub rsp, max_rooms * 4

  mov eax, [rng_state]
  mov [map_seed], eax

  xor r8d, r8d ; room count

  ; place first room
  mov rbx, rsp
  call random_room
  inc r8d

  mov r9d, 500 ; max number of attempts
: lea rbx, [rsp+r8*4]
  call random_room
  lea rbx, [rsp+r8*4]
  lea rcx, [rsp]
  mov edx, r8d
  call room_placeable
  cjmp.nz eax, >
  inc r8d
  cjmp.e r8d, max_rooms, .rooms_generated
: dcjmp.nz r9d, <<

.rooms_generated:
  ; place rooms
  xor r9d, r9d
.place_next:
  movzx eax, m8 [rsp+r9*4+0]
  movzx ebx, m8 [rsp+r9*4+1]
  movzx ecx, m8 [rsp+r9*4+2]
  movzx edx, m8 [rsp+r9*4+3]
  call place_room
  icjmp.l r9d, r8d, .place_next

  ; connect rooms
  mov r9, rsp
.next_connection:
  cjmp.l r8d, 2, .rooms_connected

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
  mov r8d, map_width
  lea r9, [tilemap]
  add r9, map_width
.next_tile:
  lea rax, [r9+r8]
  cjmp.ne m8 [rax], 0, >
  tjmp.z m8 [rax-map_width], tm_walkable, >
  mov m8 [r9+r8], tile_cliff
: icjmp.l r8d, (map_width - 1) * map_height, .next_tile

  ; randomly place walls
  mov r10d, 1000
  mov r9d, max_walls
: cjmp.z r10d, .done

  mov eax, 1
  mov ebx, map_width - 2
  call random_int
  mov r8d, eax

  mov eax, 1
  mov ebx, map_height - 2
  call random_int

  imul eax, eax, map_width
  add eax, r8d
  lea rbx, [tilemap]
  add rbx, rax
  dec r10d
  cjmp.ne m8 [rbx], 0, <
  tjmp.z m8 [rbx+map_width], tm_walkable, <
  mov m8 [rbx], tile_wall | tm_opaque
  dcjmp.nz r9d, <

.done:
  add rsp, max_rooms * 4
  .pop
  ret

place_entities:
  .push r8 r9 r10

  lea r8, [entities+4]
  mov r9d, 1

  ; generate random coords
: mov eax, 1
  mov ebx, map_width - 2
  call random_int
  mov r10d, eax
  mov eax, 1
  mov ebx, map_height - 2
  call random_int

  lea rbx, [tilemap]
  imul ecx, eax, map_width
  add ecx, r10d

  ; should be walkable and not have an entity
  tjmp.z m8 [rbx+rcx], tm_walkable, <
  tjmp.nz m8 [rbx+rcx], tm_entity, <

  ; add entity id to tile
  mov edx, r9d
  shl edx, tm_entity_shift
  or m8 [rbx+rcx], dl

  mov [r8+e_xpos], r10b
  mov [r8+e_ypos], al
  add r8, 4
  icjmp.l r9d, max_enemies + 2, <

  ; set entity type/hp and clear flags
  lea rax, [entities]
  mov m8 [rax+4+e_data], entity_char | char_hp
  mov m8 [rax+4+e_flags], 0

  mov ebx, 2
: mov m8 [rax+rbx*4+e_data], entity_crawler | crawler_hp
  mov m8 [rax+rbx*4+e_flags], 0
  icjmp.l ebx, max_enemies + 2, <

  .pop
  ret

; room dst: rbx
random_room:
  .push r8 r9 r10 r11 r12
  ; (r12) store room dst
  mov r12, rbx

  ; (r10d) randomly pick width
  mov eax, room_min_dim
  mov ebx, room_max_dim
  call random_int
  mov r10d, eax

  ; (r11d) randomly pick height
  mov eax, room_min_dim
  mov ebx, room_max_dim
  call random_int
  mov r11d, eax

  ; (r8d) randomly pick x0
  mov eax, 1
  mov ebx, map_width - 1
  sub ebx, r10d
  call random_int
  mov r8d, eax

  ; (r9d) randomly pick y0
  mov eax, 1
  mov ebx, map_height - 1
  sub ebx, r11d
  call random_int

  mov r9d, eax
  ; (r10d, r11d) compute x1, y1
  add r10d, r8d
  add r11d, r9d

  cxchg.le r8d, r10d ; if x0 > x1, swap them
  cxchg.le r9d, r11d ; if y0 > y1, swap them
  ; store the room in the rooms array
  mov [r12+0], r8b
  mov [r12+1], r9b
  mov [r12+2], r10b
  mov [r12+3], r11b
  .pop
  ret

; room: rbx, rooms: rcx, count: edx
; -> eax TODO: swap result boolean
room_placeable:
  .push r8 r9 r10 r11
  mov r8, rbx   ; room
  mov r9, rcx   ; rooms
  mov r10d, edx ; count
  mov r11, 1
: mov rbx, r8
  mov rcx, r9
  call room_intersects
  cjmp.nz eax, >
  add r9, 4
  dcjmp.nz r10d, <
  xor r11d, r11d
: mov eax, r11d
  .pop
  ret

; new room: rbx, existing room: rcx
; -> eax
room_intersects:
  xor eax, eax
  ; ax0 > bx1?
  movzx edx, m8 [rbx]
  movzx edi, m8 [rcx+2]
  cjmp.g edx, edi, >
  ; ax1 < bx0?
  movzx edx, m8 [rbx+2]
  movzx edi, m8 [rcx]
  dec edi
  cjmp.l edx, edi, >
  ; ay0 > by1?
  movzx edx, m8 [rbx+1]
  movzx edi, m8 [rcx+3]
  cjmp.g edx, edi, >
  ; ay1 < by0?
  movzx edx, m8 [rbx+3]
  movzx edi, m8 [rcx+1]
  dec edi
  cjmp.l edx, edi, >
  mov eax, 1
: ret

; x0: eax, y0: ebx, x1: ecx, y1: edx
place_room:
  cxchg.le eax, ecx ; if x0 > x1, swap them
  cxchg.le ebx, edx ; if y0 > y1, swap them
: lea rsi, [tilemap]
  imul edi, ebx, map_width
  add rsi, rdi
: mov edi, eax
: mov m8 [rsi+rdi], 1 | tm_walkable
  icjmp.l edi, ecx, <
  add rsi, map_width
  icjmp.l ebx, edx, <<
  ret

; x0: eax, y0: ebx, x1: ecx, y1: edx
place_connection:
  .push r8 r9 r10 r11
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
  .pop
  ret

; x0: eax, x1: ebx, y: ecx
place_connection_horz:
  ; (rsi) compute initial offset into the tilemap
  lea rsi, [tilemap]
  imul ecx, ecx, map_width
  add rsi, rcx
  ; (edx) compute loop delta
  mov edx, 1
  mov edi, -1
  cmp eax, ebx
  cmovg edx, edi
  add ebx, edx
: mov m8 [rsi+rax], 1 | tm_walkable
  add eax, edx
  cjmp.ne eax, ebx, <
  ret

; y0: eax, y1: ebx, x: ecx
place_connection_vert:
  ; compute p1, p2
  lea rsi, [tilemap]
  add rsi, rcx
  imul rax, rax, map_width
  imul rbx, rbx, map_width
  add rax, rsi
  add rbx, rsi
  cxchg.le rax, rbx ; if p1 > p2, swap them
: mov m8 [rax], 1 | tm_walkable
  add rax, map_width
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

; min: eax, max: ebx
; -> result: eax
random_int:
  .push r8 r9
  mov r8d, eax
  mov r9d, ebx
  sub r9d, r8d
  inc r9d
  call xorshift32
  xor edx, edx
  div r9d
  mov eax, edx
  add eax, r8d
  .pop
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
  .i16 0 0 screen_width screen_height ; x, y, width, height
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
  .i32 0x00000000 0x560000a8 0x09558002 0x00099600 0x5a0009a6 0x02a98002 0x5809a560 0xa96025a5 0x0aaa8026
  .i32 0x00000000 0x00000000 0x002a0000 0x60009580 0x5a600255 0x02556002 0x98099998 0xa6a009a6 0x00080002

cursor: .i16 0x111f 0x888f 0xf111 0xf888

font:
  .i16 0x7fff
  .i16 0x7b6f 0x749a 0x73e7 0x79e7 0x49ed 0x79cf 0x7bcf 0x4927 0x7bef 0x49ef 0x5f6f 0x3beb
  .i16 0x724f 0x3b6b 0x73cf 0x13cf 0x7a4f 0x5bed 0x7497 0x7b24 0x5aed 0x7249 0x5b7d 0x5b6f
  .i16 0x7b6f 0x13ef 0x4f6f 0x576b 0x39ce 0x2497 0x7b6d 0x2b6d 0x5f6d 0x5aad 0x25ed 0x72a7

glyphmap:
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
  .i8 2 3 4 5 6 7 8 9 10 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  .i8 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
  .i8 27 28 29 30 31 32 33 34 35 36 0 0 0 0

map_seed: .i32 0
rng_state: .i32 0
sockfd: .i32 0

; each entity has x, y, type/hp and flags
entities: .res 4
char: .res 4
.res (max_entities-2)*4

tilemap: .res screen_width * screen_height

distbuf: .res map_width * map_height

time: .i64 0
timer: .i64 0
do_timer: .i8 0

debugmode: .i8 0

selected_ability: .i8 0

str_hp: .i8 "hp"
str_dash: .i8 "dash"

put_image:
  .i8 72             ; opcode
  .i8 2              ; format (ZPixmap)
  .i16 0             ; length (0 to allow big request)
  .i32 7 + screen_width * screen_height ; actual length
.window:
  .i32 0             ; window id
.gc:
  .i32 0             ; gc id
  .i16 screen_width screen_height 0 0   ; width, height,x, y
  .i8 0 24 0 0       ; left-pad, depth
screen:
  .res screen_width * screen_height * 4
