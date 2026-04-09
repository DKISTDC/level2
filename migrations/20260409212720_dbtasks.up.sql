CREATE TABLE IF NOT EXISTS tasks (
  _id SERIAL PRIMARY KEY,
  queue TEXT NOT NULL,
  working TEXT NOT NULL,
  task_id TEXT NOT NULL,
  status TEXT NOT NULL
);

CREATE INDEX idx_tasks_queue ON tasks(queue);
CREATE INDEX idx_tasks_id ON tasks(task_id);
