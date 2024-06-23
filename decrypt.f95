program decrypt
    implicit none
    character(len=26) :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=100) :: ciphertext, keyword, plaintext
    integer :: i, n_cipher, n_key, shift

    ! input
    print *, 'Enter ciphertext (uppercase): '
    read *, ciphertext
    print *, 'Enter key (uppercase): '
    read *, keyword

    ! determine length
    n_cipher = len(trim(ciphertext))
    n_key = len(trim(keyword))

    ! decryption
    plaintext = ciphertext
    do i = 1, n_cipher
        ! shift amount
        shift = mod(index(alphabet, ciphertext(i:i)) - index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) + 26, 26) + 1
        ! replace
        plaintext(i:i) = alphabet(shift:shift)
    end do

    print *, 'Decrypted plaintext: ', trim(plaintext)

end program decrypt
