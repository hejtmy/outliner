import React, { memo } from 'react';
import { Handle, Position } from '@xyflow/react';

export default memo(function CustomNode({ data, type, selected }) {
  const accent = data.accent || '#0f766e';
  const isStep = type === 'step';
  const classes = `outline-node ${isStep ? 'is-step' : 'is-artifact'} ${selected ? 'is-selected' : ''}`;

  return (
    <div className={classes} style={{ '--node-accent': accent }}>
      <Handle className="outline-handle" type="target" position={Position.Left} />

      <div className="outline-node-body">
        <span className="outline-node-kicker">{isStep ? 'step' : 'artifact'}</span>
        <strong>{data.label}</strong>
        {isStep && data.summary && <p>{data.summary}</p>}
      </div>

      <Handle className="outline-handle" type="source" position={Position.Right} />
    </div>
  );
});
