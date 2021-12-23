echo "$(date +"%Y-%m-%dT%H:%M")" >> /home/phv/automation/hrone_cron_log.log 2>&1
curl 'https://hronewebapi.hrone.cloud/api/timeoffice/mobile/checkin/Attendance/Request' -X POST -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:95.0) Gecko/20100101 Firefox/95.0' -H 'Accept: application/json, text/plain, */*' -H 'Accept-Language: en-US,en;q=0.5' -H 'Content-Type: application/json' -H 'domainCode: app' -H "Authorization: Bearer $(cat /home/phv/automation/.hroneToken)" -H 'Origin: https://app.hrone.cloud' -H 'Connection: keep-alive' -H 'Referer: https://app.hrone.cloud/' -H 'Sec-Fetch-Dest: empty' -H 'Sec-Fetch-Mode: cors' -H 'Sec-Fetch-Site: same-site' --data-raw "{\"requestType\":\"A\",\"employeeId\":40,\"latitude\":\"\",\"longitude\":\"\",\"geoAccuracy\":\"\",\"geoLocation\":\"\",\"punchTime\":\"$(date +"%Y-%m-%dT%H:%M")\",\"uploadedPhotoOneName\":\"\",\"uploadedPhotoOnePath\":\"\",\"uploadedPhotoTwoName\":\"\",\"uploadedPhotoTwoPath\":\"\",\"attendanceSource\":\"W\",\"attendanceType\":\"Online\",\"ipAddress\":\"\"}" >> /home/phv/automation/hrone_cron_log.log 2>&1
echo "--------------------------------------------" >> /home/phv/automation/hrone_cron_log.log 2>&1