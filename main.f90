program main
    implicit none
    character(len=26) :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1024) :: plaintext, keyword, ciphertext, original_text
    integer :: i, n_plain, n_key, shift, choice

    ! input choice
    print *, ''
    print *, 'Enter 1 to encrypt, 2 to decrypt: '
    read *, choice

    ! input plaintext or ciphertext
    if (choice == 1) then
        print *, ''
        print *, 'Enter plaintext: '
        read(5, '(A)') plaintext
        original_text = plaintext
        plaintext = adjustl(uppercase(plaintext)) ! Convert to uppercase
    else if (choice == 2) then
        print *, ''
        print *, 'Enter ciphertext: '
        read(5, '(A)') ciphertext
        original_text = ciphertext
        ciphertext = adjustl(uppercase(ciphertext)) ! Convert to uppercase
    else
        print *, 'Invalid choice'
        stop
    end if

    ! input key
    print *, 'Enter key (uppercase or lowercase): '
    read *, keyword
    keyword = adjustl(uppercase(keyword)) ! Convert key to uppercase

    ! determine length of plaintext/ciphertext and key
    if (choice == 1) then
        n_plain = len_trim(plaintext)
    else if (choice == 2) then
        n_plain = len_trim(ciphertext)
    end if
    n_key = len_trim(keyword)

    ! encryption or decryption
    if (choice == 1) then
        ciphertext = plaintext
        do i = 1, n_plain
            if (index(alphabet, plaintext(i:i)) > 0) then
                ! shift amount for encryption
                shift = mod(index(alphabet, plaintext(i:i)) + index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) - 2, 26) + 1
                ! replace
                ciphertext(i:i) = alphabet(shift:shift)
            else
                ciphertext(i:i) = plaintext(i:i) ! Leave symbols and spaces unchanged
            end if
        end do
        ! Restore original case
        do i = 1, n_plain
            if (original_text(i:i) >= 'a' .and. original_text(i:i) <= 'z') then
                ciphertext(i:i) = lowercase(ciphertext(i:i))
            end if
        end do
        print *, 'Encrypted ciphertext: ', trim(ciphertext)
    else if (choice == 2) then
        plaintext = ciphertext
        do i = 1, n_plain
            if (index(alphabet, ciphertext(i:i)) > 0) then
                ! shift amount for decryption
                shift = mod(index(alphabet, ciphertext(i:i)) - index(alphabet, keyword(mod(i-1, n_key)+1:mod(i-1, n_key)+1)) + 26, 26) + 1
                ! replace
                plaintext(i:i) = alphabet(shift:shift)
            else
                plaintext(i:i) = ciphertext(i:i) ! Leave symbols and spaces unchanged
            end if
        end do
        ! Restore original case
        do i = 1, n_plain
            if (original_text(i:i) >= 'a' .and. original_text(i:i) <= 'z') then
                plaintext(i:i) = lowercase(plaintext(i:i))
            end if
        end do
        print *, 'Decrypted plaintext: ', trim(plaintext)
    end if

contains

    function uppercase(text) result(upper_text)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: upper_text
        integer :: j
        upper_text = text
        do j = 1, len(text)
            if (text(j:j) >= 'a' .and. text(j:j) <= 'z') then
                upper_text(j:j) = char(iachar(text(j:j)) - 32)
            else
                upper_text(j:j) = text(j:j)
            end if
        end do
    end function uppercase

    function lowercase(text) result(lower_text)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower_text
        integer :: j
        lower_text = text
        do j = 1, len(text)
            if (text(j:j) >= 'A' .and. text(j:j) <= 'Z') then
                lower_text(j:j) = char(iachar(text(j:j)) + 32)
            else
                lower_text(j:j) = text(j:j)
            end if
        end do
    end function lowercase

end program main