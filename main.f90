program main
    implicit none
    character(len=26) :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1024) :: plaintext, keyword, ciphertext
    integer :: i, n_plain, n_key, shift, choice

    ! input choice
    print *, ''
    print *, 'Due to my skill issue, all text inputs must be normal alphabets and uppercase '
    print *, 'Maximum text input length: 1024'
    print *, ''
    print *, 'Enter 1 to encrypt, 2 to decrypt: '
    read *, choice

    ! input plaintext or ciphertext
    if (choice == 1) then
        print *, ''
        print *, 'Enter plaintext: '
        read *, plaintext
    else if (choice == 2) then
        print *, ''
        print *, 'Enter ciphertext: '
        read *, ciphertext
    else
        print *, 'Invalid choice'
        stop
    end if

    ! input key
    print *, 'Enter key (uppercase): '
    read *, keyword

    ! determine length of plaintext/ciphertext and key
    if (choice == 1) then
        n_plain = len(trim(plaintext))
    else if (choice == 2) then
        n_plain = len(trim(ciphertext))
    end if
    n_key = len(trim(keyword))

    ! encryption or decryption
    if (choice == 1) then
        ciphertext = plaintext
        do i = 1, n_plain
            ! shift amount for encryption
            shift = mod(index(alphabet, plaintext(i:i)) + index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) - 2, 26) + 1
            ! replace
            ciphertext(i:i) = alphabet(shift:shift)
        end do
        print *, 'Encrypted ciphertext: ', trim(ciphertext)
    else if (choice == 2) then
        plaintext = ciphertext
        do i = 1, n_plain
            ! shift amount for decryption
            shift = mod(index(alphabet, ciphertext(i:i)) - index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) + 26, 26) + 1
            ! replace
            plaintext(i:i) = alphabet(shift:shift)
        end do
        print *, 'Decrypted plaintext: ', trim(plaintext)
    end if

end program main