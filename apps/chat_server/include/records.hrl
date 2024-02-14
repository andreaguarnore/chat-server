-record(user, {name,
               room=nil}). % name of the current room of the user, or `nil`

-record(room, {owner,             % the room creator
               type=public,       % either `public` or `private`
               members=[],        % users that are invited to the room
               participants=[]}). % users currently in room

