-- Program.Id => Program[Observation[Step[_]]

  SELECT p.program_id,
         p.title,
         o.observation_id,
         o.title,
         s.index,
         s.instrument,
         s.step_type,
         sg.gcal_lamp,
         sg.shutter,
         sc.offset_p,
         sc.offset_q
    FROM program p
         LEFT OUTER JOIN observation o ON o.program_id = p.program_id
         LEFT OUTER JOIN step s ON s.observation_id = o.observation_id
         LEFT OUTER JOIN step_gcal sg
            ON sg.observation_id = s.observation_id AND sg.index = s.index
         LEFT OUTER JOIN step_science sc
            ON sc.observation_id = s.observation_id AND sc.index = s.index
   WHERE p.program_id = 'Benoit'
ORDER BY observation_id, index;

-- Program.Id => Program[Observation[Nothing]] or Program[Observation.Id]

  SELECT p.program_id,
         p.title,
         o.observation_id,
         o.title
    FROM program p LEFT OUTER JOIN observation o ON o.program_id = p.program_id
   WHERE p.program_id = 'Benoit'
ORDER BY observation_id, index;

-- Observation.Id => Observation[Step[_]]

  SELECT o.observation_id,
         o.title,
         s.index,
         s.instrument,
         s.step_type,
         sg.gcal_lamp,
         sg.shutter,
         sc.offset_p,
         sc.offset_q
    FROM observation o
         LEFT OUTER JOIN step s ON s.observation_id = o.observation_id
         LEFT OUTER JOIN step_gcal sg
            ON sg.observation_id = s.observation_id AND sg.index = s.index
         LEFT OUTER JOIN step_science sc
            ON sc.observation_id = s.observation_id AND sc.index = s.index
   WHERE o.program_id = 'Benoit' AND o.observation_index = 12
ORDER BY observation_id, index

;



