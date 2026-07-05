program main
  !! TUPÃ electromagnetic field transient solver.
  !!
  !! **Entry point for command-line execution.**
  !!
  !! Loads a JSON study file and runs the complete electromagnetic analysis pipeline:
  !!
  !! **Usage:**
  !!   fpm run -- <study.json>
  !!   ./tupa <study.json>
  !!
  !! **Input:**
  !! - `<study.json>` — path to a JSON file describing the electromagnetic study
  !!   - See [JSON format details](../src/Tupa.f90) for schema
  !!
  !! **Output:**
  !! - Printed summary of study geometry (nodes, materials, elements)
  !! - (Future) CSV and/or JSON files with frequency-domain solution
  !!
  !! **Example JSON:**
  !!   ```json
  !!   {
  !!     "title": "Buried conductor study",
  !!     "soil": {
  !!       "permittivity": 10.0,
  !!       "permeability": 1.0,
  !!       "conductivity": 0.01
  !!     },
  !!     "nodes": [
  !!       {"id": "node1", "position": [0, 0, 0]},
  !!       {"id": "node2", "position": [10, 0, -0.5]}
  !!     ],
  !!     "elements": [
  !!       {
  !!         "type": "line",
  !!         "id": "line1",
  !!         "from": "node1",
  !!         "to": "node2",
  !!         "radius": 0.005,
  !!         "segments": 5,
  !!         "material": "copper"
  !!       }
  !!     ]
  !!   }
  !!   ```
  use tupa, only: runFromFile
  use mError, only: raiseError
  implicit none

  character(len=512) :: filename
  !! Path to the JSON study file (from command-line argument)
  integer :: ios
  !! Status flag for command-line argument retrieval

  call get_command_argument(1, filename, status=ios)
  if (ios /= 0 .or. len_trim(filename) == 0) then
    print *, "Usage: tupa <study.json>"
    call raiseError("missing study file argument")
  end if

  call runFromFile(trim(filename))
end program main
