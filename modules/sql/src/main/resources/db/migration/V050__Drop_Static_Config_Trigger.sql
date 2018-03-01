--
-- Delete trigger for static_config cleanup, since static_config no longer exists.
--

DROP TRIGGER delete_static_config ON observation;
