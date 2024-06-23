program encrypt
    implicit none
    character(len=26) :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=100) :: plaintext, keyword, ciphertext
    integer :: i, n_plain, n_key, shift

    ! input
    print *, 'Enter plaintext (uppercase): '
    read *, plaintext
    print *, 'Enter key (uppercase): '
    read *, keyword

    ! determine length
    n_plain = len(trim(plaintext))
    n_key = len(trim(keyword))

    ! encryption
    ciphertext = plaintext
    do i = 1, n_plain
        ! shift amount
        shift = mod(index(alphabet, plaintext(i:i)) + index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) - 2, 26) + 1
        ! replace
        ciphertext(i:i) = alphabet(shift:shift)
    end do

    print *, 'Encrypted ciphertext: ', trim(ciphertext)

end program encrypt
