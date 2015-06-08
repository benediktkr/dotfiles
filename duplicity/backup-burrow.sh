set -e
export PASSPHRASE=""
KEYID="0x61FD1056812214D6"

/usr/bin/duplicity \
    --verbosity 4 \
    --full-if-older-than 7D \
    --include /home/benedikt/documents \
    --include /home/benedikt/projects \
    --include /home/benedikt/.ssh \
    --include /home/benedikt/scrots \
    --include /home/benedikt/.gnupg \
    --exclude '**' \
    --volsize 1000 \
    --max-blocksize 16777216 \
    --gpg-options "--bzip2-compress-level=0" \
    /home/benedikt/ sftp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey" \
    --encrypt-key $KEYID \
    --sign-key $KEYID

/usr/bin/duplicity collection-status sftp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey"

/usr/bin/duplicity remove-older-than "8D" \
    --force \
    sftp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey"

unset PASSPHRASE
