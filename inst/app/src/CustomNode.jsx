import React, { memo } from 'react';
import { Handle, Position } from '@xyflow/react';

export default memo(({ data, type }) => {
  const isFunction = type === 'function';
  const color = data.color || '#777';

  // User requested: 
  // Data enters function from Left -> Function Target is Left
  // Data returned from function exists on Right -> Function Source is Right

  // For consistency in a Left-to-Right flow:
  // Objects also accept from Left and output to Right using same logic

  return (
    <div style={{
      padding: '10px',
      border: `2px solid ${color}`,
      borderRadius: isFunction ? '5px' : '20px',
      background: '#fff',
      minWidth: isFunction ? '150px' : '100px',
      textAlign: 'center',
      boxShadow: '0 2px 5px rgba(0,0,0,0.1)'
    }}>
      <Handle
        type="target"
        position={Position.Left}
        style={{ background: '#555' }}
      />

      <div style={{
        fontWeight: 'bold',
        marginBottom: '5px',
        color: isFunction ? '#000' : '#555'
      }}>
        {data.label}
      </div>

      {isFunction && data.short && (
        <div style={{ fontSize: '0.8em', color: '#555', textAlign: 'left' }}>
          {data.short}
        </div>
      )}

      <Handle
        type="source"
        position={Position.Right}
        style={{ background: '#555' }}
      />
    </div>
  );
});
