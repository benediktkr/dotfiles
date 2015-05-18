set -e

export PASSPHRASE=""
KEYID="0x61FD1056812214D6"
repDate=`date +%Y%m%d`;

echo "Performing backup.."

/usr/bin/duplicity \
    --include /home/benedikt/documents \
    --include /home/benedikt/projects \
    --include /home/benedikt/.ssh \
    --exclude '**' \
    --volsize 1000 \
    /home/benedikt/ scp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey" \
    --encrypt-key $KEYID \
    --sign-key $KEYID # \
#    > /home/benedikt/.duplicity/burrow_${repDate}.log

echo "Getting list of backups.."

/usr/bin/duplicity \
    collection-status \
    /home/benedikt/ scp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey" \
    >> /home/benedikt/.duplicity/burrow_${repDate}.log

# Mail me the results
#mail -t localuser -s "localmachine Backup Incremental (Admin)" < /home/localuser/Documents/BackupLogs/Incremental_Backup_Log_${repDate}_localmachine_admin.log


# Do cleanup after the backup
/usr/bin/duplicity \
    remove-older-than 14D \
    -v9 \
    --force \
    /home/benedikt/ scp://duplicity@freespace.sudo.is/burrow/ \
    --ssh-options="-oIdentityFile=/home/benedikt/.ssh/backupkey" \

unset PASSPHRASE
