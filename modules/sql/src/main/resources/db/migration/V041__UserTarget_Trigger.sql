--
-- This migration adds a trigger to cleanup the corresponding target when a
-- user target is deleted.
--

-- User targets are automatically deleted when the observation in which they
-- are contained is deleted.  There is no reference from target back to
-- user target though, so a deleted user target would leave garbage in the
-- target table were it not for this trigger.

CREATE FUNCTION delete_user_target_reference() RETURNS trigger AS $delete_user_target_reference$
  BEGIN
    DELETE FROM target where id = OLD.target_id;
    RETURN OLD;
  END;
$delete_user_target_reference$ LANGUAGE plpgsql;

CREATE TRIGGER delete_user_target_reference AFTER DELETE ON user_target
  FOR EACH ROW EXECUTE PROCEDURE delete_user_target_reference();
