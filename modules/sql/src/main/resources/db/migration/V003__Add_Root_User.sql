
-- Set the root user's password to '' if it hasn't been set. This will allow the
-- first administrator to log in.
UPDATE gem_user
SET    md5 = md5('')
WHERE  id  = 'root'
AND    md5 = ''
