package edu.gemini.epics.acm;

import java.time.Instant;

public interface TimestampProvider {
    Instant now();

    TimestampProvider Default = () -> Instant.now();
}
