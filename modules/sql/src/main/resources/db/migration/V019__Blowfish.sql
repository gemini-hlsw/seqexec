
-- We need the stamdard crypto extensions.
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Hash column needs to be wider, and renamed.
ALTER TABLE gem_user ALTER  md5 TYPE text;
ALTER TABLE gem_user RENAME md5 TO hash;

-- Reset the root password to the empty string.
-- Other users can no longer log in, but there shouldn't be any.
UPDATE gem_user
  SET   hash = crypt('', gen_salt('bf', 10))
  WHERE id   = 'root'
