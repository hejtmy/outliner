import React, { useCallback, useEffect, useState, useMemo } from 'react';
import { ReactFlow, Background, Controls, MiniMap, useNodesState, useEdgesState, addEdge, Panel, MarkerType } from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import dagre from 'dagre';
import CustomNode from './CustomNode';

const nodeTypes = {
  function: CustomNode,
  object: CustomNode,
};

// Simple color palette for distinct flows
const COLORS = [
  '#007bff', // Blue
  '#28a745', // Green
  '#dc3545', // Red
  '#ffc107', // Yellow
  '#17a2b8', // Cyan
  '#6610f2', // Purple
  '#fd7e14', // Orange
  '#e83e8c', // Pink
];

const getLayoutedElements = (nodes, edges, direction = 'TB') => {
  const dagreGraph = new dagre.graphlib.Graph();
  dagreGraph.setDefaultEdgeLabel(() => ({}));

  dagreGraph.setGraph({ rankdir: direction, nodesep: 50, ranksep: 80 });

  nodes.forEach((node) => {
    // Objects might be smaller, functions wider
    const w = node.type === 'function' ? 180 : 120;
    const h = 60;
    dagreGraph.setNode(node.id, { width: w, height: h });
  });

  edges.forEach((edge) => {
    dagreGraph.setEdge(edge.source, edge.target);
  });

  dagre.layout(dagreGraph);

  const newNodes = nodes.map((node) => {
    const nodeWithPosition = dagreGraph.node(node.id);
    return {
      ...node,
      position: {
        x: nodeWithPosition.x - (node.type === 'function' ? 90 : 60),
        y: nodeWithPosition.y - 30,
      },
      targetPosition: 'left',
      sourcePosition: 'right',
    };
  });

  return { nodes: newNodes, edges };
};

// Helper to assign colors to connected components
const assignColors = (nodes, edges) => {
  const adj = {};
  nodes.forEach(n => adj[n.id] = []);
  edges.forEach(e => {
    if (adj[e.source]) adj[e.source].push(e.target);
    if (adj[e.target]) adj[e.target].push(e.source);
  });

  const visited = new Set();
  const coloredNodes = [...nodes];
  let colorIdx = 0;

  for (const node of nodes) {
    if (!visited.has(node.id)) {
      const color = COLORS[colorIdx % COLORS.length];
      colorIdx++;

      const queue = [node.id];
      visited.add(node.id);
      while (queue.length > 0) {
        const curr = queue.shift();
        const nIndex = coloredNodes.findIndex(n => n.id === curr);
        if (nIndex >= 0) {
          coloredNodes[nIndex].data = { ...coloredNodes[nIndex].data, color };
        }

        if (adj[curr]) {
          adj[curr].forEach(neighbor => {
            if (!visited.has(neighbor)) {
              visited.add(neighbor);
              queue.push(neighbor);
            }
          });
        }
      }
    }
  }
  return coloredNodes;
};

const defaultEdgeOptions = {
  type: 'smoothstep',
  markerEnd: {
    type: MarkerType.ArrowClosed,
    width: 20,
    height: 20,
  },
  style: {
    strokeWidth: 2,
    stroke: '#555',
  },
  animated: true,
};

export default function App() {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [selectedNode, setSelectedNode] = useState(null);
  const [registries, setRegistries] = useState({ functions: [], objects: [] });

  useEffect(() => {
    const fetchData = async () => {
      let data = { nodes: [], edges: [] };
      if (window.outlinerData) {
        data = window.outlinerData;
      } else {
        try {
          const res = await fetch('/data.json');
          if (res.ok) data = await res.json();
        } catch (e) {
          console.log("No data found");
        }
      }

      if (data.nodes && data.nodes.length > 0) {
        // 1. Colorize
        const coloredNodes = assignColors(data.nodes, data.edges || []);

        // 2. Layout
        const layouted = getLayoutedElements(coloredNodes, data.edges || [], 'TB');
        setNodes(layouted.nodes);
        setEdges(layouted.edges);

        // 3. Registries
        const funcs = layouted.nodes.filter(n => n.type === 'function').map(n => ({
          id: n.id,
          label: n.data.label,
          desc: n.data.short || "No description available",
          color: n.data.color
        }));

        const objs = layouted.nodes.filter(n => n.type === 'object').map(n => ({
          id: n.id,
          label: n.data.label,
          color: n.data.color
        }));

        setRegistries({ functions: funcs, objects: objs });
      }
    };

    fetchData();
  }, [setNodes, setEdges]);

  const onConnect = useCallback(
    (params) => setEdges((eds) => addEdge(params, eds)),
    [setEdges],
  );

  const onNodeClick = useCallback((event, node) => {
    setSelectedNode(node);
  }, []);

  const onPaneClick = useCallback(() => {
    setSelectedNode(null);
  }, []);

  return (
    <div style={{ display: 'flex', width: '100vw', height: '100vh', overflow: 'hidden' }}>
      {/* Left Sidebar: Functions */}
      <div style={{
        width: '250px',
        borderRight: '1px solid #ccc',
        padding: '15px',
        background: '#f8f9fa',
        display: 'flex',
        flexDirection: 'column',
        zIndex: 10,
        overflowY: 'auto'
      }}>
        <h3 style={{ borderBottom: '1px solid #999', paddingBottom: '10px' }}>Functions</h3>

        <ul style={{ listStyle: 'none', padding: 0 }}>
          {registries.functions.map((func, i) => (
            <li
              key={i}
              onClick={() => {
                const node = nodes.find(n => n.id === func.id);
                if (node) setSelectedNode(node);
              }}
              style={{
                marginBottom: '10px',
                padding: '8px',
                borderLeft: `4px solid ${func.color}`,
                background: '#fff',
                cursor: 'pointer',
                boxShadow: '0 1px 2px rgba(0,0,0,0.1)'
              }}
              onMouseEnter={(e) => e.currentTarget.style.background = '#f0f0f0'}
              onMouseLeave={(e) => e.currentTarget.style.background = '#fff'}
            >
              <strong>{func.label}</strong>
            </li>
          ))}
        </ul>

        {selectedNode && selectedNode.type === 'function' && (
          <div style={{ marginTop: 'auto', paddingTop: '20px', borderTop: '2px solid #ccc' }}>
            <h4 style={{ color: selectedNode.data.color }}>{selectedNode.data.label}</h4>
            <p style={{ fontSize: '0.9em' }}><strong>Summary:</strong> {selectedNode.data.short}</p>
            <details>
              <summary>More</summary>
              <p style={{ fontSize: '0.8em', whiteSpace: 'pre-wrap' }}>{selectedNode.data.long}</p>
            </details>
            <button onClick={() => setSelectedNode(null)} style={{ marginTop: '10px' }}>Close</button>
          </div>
        )}
      </div>

      {/* Main Canvas */}
      <div style={{ flex: 1, height: '100%', position: 'relative' }}>
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onConnect={onConnect}
          nodeTypes={nodeTypes}
          onNodeClick={onNodeClick}
          onPaneClick={onPaneClick}
          defaultEdgeOptions={defaultEdgeOptions}
          fitView
        >
          <Controls />
          <MiniMap />
          <Background variant="dots" gap={12} size={1} />
        </ReactFlow>
      </div>

      {/* Right Sidebar: Data Objects */}
      <div style={{
        width: '200px',
        borderLeft: '1px solid #ccc',
        padding: '15px',
        background: '#fff',
        display: 'flex',
        flexDirection: 'column',
        zIndex: 10,
        overflowY: 'auto'
      }}>
        <h3 style={{ borderBottom: '1px solid #999', paddingBottom: '10px' }}>Data</h3>
        <ul style={{ listStyle: 'none', padding: 0 }}>
          {registries.objects.map((obj, i) => (
            <li
              key={i}
              onClick={() => {
                const node = nodes.find(n => n.id === obj.id);
                if (node) setSelectedNode(node);
              }}
              style={{
                marginBottom: '10px',
                padding: '8px',
                borderRight: `4px solid ${obj.color}`,
                background: '#fefefe',
                cursor: 'pointer',
                fontSize: '0.9em',
                border: '1px solid #eee',
                borderRightWidth: '4px'
              }}
              onMouseEnter={(e) => e.currentTarget.style.background = '#f0f0f0'}
              onMouseLeave={(e) => e.currentTarget.style.background = '#fefefe'}
            >
              {obj.label}
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}
