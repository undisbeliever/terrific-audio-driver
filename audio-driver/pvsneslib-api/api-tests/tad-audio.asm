; Including `tad-audio.asm` instead of copying it so there is only 1 copy in the repo.

; Custom default values
TAD_DEFAULT_FLAGS = TAD_Flags__RESET_GLOBAL_VOLUMES_ON_SONG_START
TAD_DEFAULT_AUDIO_MODE = TAD_AudioMode__SURROUND
TAD_DEFAULT_TRANSFER_PER_FRAME = 500

.include "../tad-audio.asm"

; MUST NOT write any code after this include, the .bank value has been reset to 0

